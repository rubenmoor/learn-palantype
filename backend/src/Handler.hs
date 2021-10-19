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

import           Common.Api              (PloverCfg (..), Routes, keyMapToPloverCfg)
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
import Palantype.Common.RawSteno (RawSteno)

handlers :: ServerT Routes '[] Snap
handlers =
  (      handleConfigNew
  )

newtype KeysMapJSON = KeysMapJSON { unKeysMapJSON :: Map RawSteno [Text]}

instance FromJSON KeysMapJSON where
  parseJSON (Array values) =
    let acc :: Map RawSteno [Text] -> Value -> Parser (Map RawSteno [Text])
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
      let pLang = case system of
            "Palantype"    -> pEN
            "Palantype DE" -> pDE
      in pure $ keyMapToPloverCfg pLang
                                  stenoKeys
                                  (Text.pack system)
                                  (Text.pack machine)
