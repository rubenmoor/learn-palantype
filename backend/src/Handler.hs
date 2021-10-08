{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Handler
  ( handlers
  ) where

import           Common.Alphabet         (PTChar (..), PTChord (..))
import           Common.Api              (Hs (..), PloverCfg (..), Routes, keyMapToPloverCfg)
import           Control.Applicative     (Alternative ((<|>)),
                                          Applicative (pure, (*>), (<*>)))
import           Control.Category        (Category ((.)))
import           Control.Monad           (Monad (fail), foldM, unless,
                                          when)
import           Control.Monad.Except    (MonadError (throwError), runExcept)
import           Data.Aeson              (FromJSON (..), Value (Array))
import qualified Data.Aeson              as Json
import           Data.Aeson.Types        (Parser, typeMismatch)
import           Data.Bool               (Bool (..))
import qualified Data.ByteString.Char8   as Char8
import qualified Data.ByteString.Lazy    as Lazy
import           Data.Char               (Char)
import qualified Data.ConfigFile         as CfgParser
import           Data.Either             (Either (..))
import           Data.Eq                 ((==))
import           Data.Foldable           (concat, Foldable (toList))
import           Data.Function           (($))
import           Data.Functor            (void, (<$>))
import           Data.List               (drop, findIndex, (!!))
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Maybe              (Maybe (..))
import           Data.Monoid             ((<>))
import           Data.String             (String)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Lazy          as TextLazy
import qualified Data.Text.Lazy.Encoding as TextLazy
import           Data.Traversable        (for)
import           Data.Tuple              (fst)
import           GHC.Num                 (Num ((+)))
import           GHC.Show                (Show (show))
import           Prelude                 (Applicative ((<*)), snd)
import           Servant                 ((:<|>) (..))
import           Servant.Server          (HasServer (ServerT),
                                          ServantErr (errBody), err400)
import qualified Servant.Server          as Snap (throwError)
import           Snap.Core               (Snap)
import           Text.Parsec             (Parsec, anyChar, char, eof, getState, many1, runParser,
                                          sepBy1, setState, try,
                                          updateState)

handlers :: ServerT Routes '[] Snap
handlers =
  (      handleConfigNew
    :<|> handleLookupSteno
  )
  :<|> handleStenoExpressions

newtype KeysMapJSON = KeysMapJSON { unKeysMapJSON :: Map String [String]}

instance FromJSON KeysMapJSON where
  parseJSON (Array values) =
    let acc :: Map String [String] -> Value -> Parser (Map String [String])
        acc m a@(Array xs) = do
          (key, value) <- case toList xs of
            [l1, l2] -> (,) <$> parseJSON l1 <*> parseJSON l2
            _        -> typeMismatch "values" a
          pure $ Map.insert key value m
        acc _ invalid = typeMismatch "key map" invalid
        map = foldM acc Map.empty values
    in  KeysMapJSON <$> map
  parseJSON invalid = typeMismatch "key map" invalid

handleConfigNew :: String -> Snap PloverCfg
handleConfigNew str = do
  let eCfg = runExcept $ do

        parser <- CfgParser.readstring CfgParser.emptyCP str
        let parse = CfgParser.get parser
        systemName <- parse "System" "name"
        machineType <- parse "Machine Configuration" "machine_type"
        unless (machineType == "Keyboard") $
          throwError (CfgParser.ParseError $ "machine: " <> machineType,
                      "Sorry! Only keyboard is supported.")
        keyMapStr <- parse ("System: " <> systemName) ("keymap[" <> machineType <> "]")
        m <- case Json.eitherDecode $ Lazy.fromStrict $ Char8.pack keyMapStr of
          Left msg -> throwError (CfgParser.ParseError msg, "could not decode keymap")
          Right km -> pure (km :: KeysMapJSON)
        pure
          ( systemName
          , machineType
          , Map.toList $ unKeysMapJSON m
          )

  case eCfg of
    Left err -> Snap.throwError $
      err400 { errBody = Lazy.fromStrict  $ Char8.pack $ show err }
    Right (system, machine, stenoKeys) ->
      pure $ keyMapToPloverCfg stenoKeys
                               (Text.pack system)
                               (Text.pack machine)

parserChord :: Parsec String ([(Char, PTChar)], Bool) [PTChar]
parserChord = do

  let lsChars =
        [ ('S', LeftS)
        , ('C', LeftC)
        , ('P', LeftP)
        , ('T', LeftT)
        , ('H', LeftH)
        , ('+', LeftCross)
        , ('M', LeftM)
        , ('F', LeftF)
        , ('R', LeftR)
        , ('N', LeftN)
        , ('L', LeftL)
        , ('Y', LeftY)
        , ('O', LeftO)
        , ('E', LeftE)
        , ('|', LeftPipe)
        , ('|', RightPipe)
        , ('A', RightA)
        , ('U', RightU)
        , ('I', MiddleI)
        , ('^', RightPoint)
        , ('N', RightN)
        , ('L', RightL)
        , ('C', RightC)
        , ('M', RightM)
        , ('F', RightF)
        , ('R', RightR)
        , ('P', RightP)
        , ('T', RightT)
        , ('+', RightCross)
        , ('S', RightS)
        , ('H', RightH)
        , ('e', RightE)
        ]
  setState (lsChars, False)

  let parserKey = do
        (ls, _) <- getState
        c <- anyChar
        case findIndex ((==) c . fst) ls of
            Nothing -> fail "malformed"
            Just i  -> do
              updateState $ (drop (i + 1) ls,) . snd
              pure $ snd $ ls !! i

      parserHypen = do
        (ls, foundHyphen) <- getState
        when foundHyphen $ fail "malformed"
        void $ char '-'
        setState (drop 15 ls, True)
  many1 ( try parserKey <|> (parserHypen *> parserKey))

parseSteno :: String -> Either Text [PTChord]
parseSteno str = do
  let  parser = sepBy1 parserChord (char '/') <* eof
  case runParser parser ([], False) "client request" str of
    -- Left err -> Snap.throwError $
    --   err400 { errBody = Lazy.fromStrict $ Char8.pack $ show err }
    Left  err -> Left  $ Text.pack $ show err
    Right ls  -> Right $ PTChord <$> ls

handleLookupSteno :: [Text] -> Snap [PTChord]
handleLookupSteno str = do
  -- look up in database
  -- unless found:
  --   parse
  --   if fail: snap error
  --   else   : store to database
  --            return
  ls <-
    for str $ \s -> do
      -- TODO: database lookup
      case parseSteno (Text.unpack s) of
        Left  err -> Snap.throwError $
          err400 { errBody = TextLazy.encodeUtf8 $ TextLazy.fromStrict err }
        Right chords -> do
          -- TODO: store in db
          pure chords
  pure $ concat ls

handleStenoExpressions :: Snap Hs
handleStenoExpressions = pure $ Hs ""
  -- TODO: get all from database
  --       render file
