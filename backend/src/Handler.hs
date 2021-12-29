{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Handler
    ( handlers
    ) where

import           Common.Api                     (DictId (..),  PloverSystemCfg
                                                , Routes
                                                , keyMapToPloverCfg
                                                , CfgName (CNFile)
                                                )
import           Control.Applicative            ( Applicative((<*>), pure) )
import           Control.Category               ((<<<),  Category((.)) )
import           Control.Monad                  ( foldM
                                                , unless

                                                )
import           Control.Monad.Except           ( MonadError(throwError)
                                                , runExcept
                                                )
import           Data.Aeson                     ( FromJSON(..)
                                                , Value(Array)
                                                )
import qualified Data.Aeson                    as Json
import           Data.Aeson.Types               ( Parser
                                                , typeMismatch
                                                )
import qualified Data.ByteString.Char8         as Char8
import qualified Data.ByteString.Lazy          as LazyBS
import qualified Data.Text.Lazy          as LazyT
import qualified Data.Text.Lazy.IO          as LazyT
import qualified Data.ConfigFile               as CfgParser
import           Data.Either                    ( Either(..)
                                                , either
                                                )
import           Data.Eq                        ( (==) )
import           Data.Foldable                  ( Foldable(elem, toList)
                                                , foldl'
                                                )
import           Data.Function                  ( ($)
                                                , const
                                                )
import           Data.Functor                   (Functor(fmap),  (<$>) )
import           Data.List                      (filter, take,  (++) )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( Maybe(..) )
import           Data.Monoid                    ( (<>) )
import           Data.String                    ( String )
import           Data.Text                      (toUpper,  Text )
import qualified Data.Text                     as Text
import           GHC.Show                       ( Show(show) )
import           Palantype.Common               (Lang (..),  KeyIndex
                                                , Palantype(keyIndex)
                                                )
import           Palantype.Common.RawSteno      (RawSteno (..)
                                                , parseStenoKey
                                                )
import qualified Palantype.DE.Keys             as DE
import qualified Palantype.EN.Keys             as EN
import           Servant.API                    ( (:<|>)(..) )
import           Servant.Server                 ( HasServer(ServerT)
                                                , ServantErr(errBody)
                                                , err400

                                                )
import qualified Servant.Server                as Snap
                                                ( throwError )
import           Snap.Core                      ( Snap )
import Data.Int (Int)
import GHC.Err (error)
import Control.Monad.IO.Class (MonadIO(liftIO))
import TextShow (TextShow(showt))

handlers :: ServerT Routes '[] Snap
handlers = handleConfigNew :<|> handleDict

-- handleDictTop2k

handleDict :: Int -> DictId -> Snap (Map RawSteno Text, Map Text [RawSteno])
handleDict n = \case
    DictSimpleSingle -> readDict $ \(raw, word) -> toUpper word == showt raw
    DictSimpleMulti -> pure (Map.empty, Map.empty)
    DictReplsSingle -> pure (Map.empty, Map.empty)
    DictReplsMulti -> pure (Map.empty, Map.empty)
    DictCodaHR -> pure (Map.empty, Map.empty)
  where

    file = "palantype-DE.txt"

    readDict pred = liftIO $ do
      ls <-   take n
            . filter pred
            . fmap (parseLine <<< LazyT.toStrict)
            . LazyT.lines
          <$> LazyT.readFile file

      let acc (mapStenoWord, mapWordStenos) (raw, word) =
              ( Map.insert raw word mapStenoWord
              , Map.insertWith (++) word [raw] mapWordStenos
              )
          -- mWordStenos = foldl' acc HashMap.empty $ HashMap.toList mStenoWord
      pure $ foldl' acc (Map.empty, Map.empty) ls

    parseLine line = case Text.splitOn " " line of
        [raw, word] -> (RawSteno raw, word)
        _ -> error $ "file " <> file <> " wrong format: " <> Text.unpack line

-- handleConfigNew

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

handleConfigNew :: String -> Snap (Lang, PloverSystemCfg)
handleConfigNew str = do
    let
        eCfg = runExcept $ do

            parser <- CfgParser.readstring CfgParser.emptyCP str
            let parse = CfgParser.get parser
            systemName  <- parse "System" "name"
            machineType <- parse "Machine Configuration" "machine_type"
            unless (machineType == "Keyboard") $ throwError
                ( CfgParser.ParseError $ "machine: " <> machineType
                , "Sorry! Only keyboard is supported."
                )
            keyMapStr <- parse ("System: " <> systemName)
                               ("keymap[" <> machineType <> "]")
            m <-
                case
                    Json.eitherDecode $ LazyBS.fromStrict $ Char8.pack keyMapStr
                of
                    Left msg ->
                        throwError
                            ( CfgParser.ParseError msg
                            , "could not decode keymap"
                            )
                    Right km -> pure (km :: KeysMapJSON)
            pure (systemName, machineType, Map.toList $ unKeysMapJSON m)

    case eCfg of
        Left err -> Snap.throwError
            $ err400 { errBody = LazyBS.fromStrict $ Char8.pack $ show err }
        Right (system, machine, stenoKeys) -> do

            lang <- case system of
                "Palantype"    -> pure EN
                "Palantype DE" -> pure DE
                _              -> Snap.throwError $ err400
                    { errBody = LazyBS.fromStrict
                                $  Char8.pack
                                $  "System "
                                <> system
                                <> " not implemented."
                    }

            let
                rawToIndex raw = case lang of
                    EN -> either (const Nothing) (Just . keyIndex)
                        $ parseStenoKey @EN.Key raw
                    DE -> either (const Nothing) (Just . keyIndex)
                        $ parseStenoKey @DE.Key raw

                acc
                    :: ([(KeyIndex, [Text])], [RawSteno])
                    -> (RawSteno, [Text])
                    -> ([(KeyIndex, [Text])], [RawSteno])
                acc (ls, uSteno) (raw, plovers) = case rawToIndex raw of
                    Just i  -> ((i, plovers) : ls, uSteno)
                    Nothing | raw `elem` ["no-op", "arpeggiate"] -> (ls, uSteno)
                    Nothing -> (ls, raw : uSteno)

                (lsIndexPlovers, unrecognizedStenos) =
                    foldl' acc ([], []) stenoKeys

            pure
                ( lang
                , keyMapToPloverCfg lsIndexPlovers
                                    unrecognizedStenos
                                    (Text.pack machine)
                                    CNFile
                )
