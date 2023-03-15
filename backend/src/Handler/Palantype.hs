{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Palantype
    ( handlers
    )
where

import           Common.Api                     ( RoutesPalantype
                                                )
import           Common.PloverConfig            ( CfgName(CNFile)
                                                , PloverSystemCfg
                                                , keyMapToPloverCfg
                                                )
import           Control.Applicative            ( Applicative((<*>), pure) )
import           Control.Category               ( Category((.)) )
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
import qualified Data.ByteString.Lazy          as LazyBS
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
import           Data.Functor                   ( (<$>) )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( Maybe(..)
                                                )
import           Data.Monoid                    ( (<>) )
import           Data.String                    ( String )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           GHC.Show                       ( Show(show) )
import Palantype.Common
    ( KeyIndex, keyIndex, SystemLang(..), RawSteno, parseStenoKey )
import qualified Palantype.DE.Keys             as DE
import qualified Palantype.EN.Keys             as EN
import           Servant.Server                 ( ServantErr(errBody)
                                                , err400
                                                , HasServer(ServerT)
                                                )
import qualified Servant.Server                as Snap
                                                ( throwError )
import           Snap.Core                      ( MonadSnap, setHeader, modifyResponse )
import qualified Data.ByteString.UTF8 as BSU

handlers :: MonadSnap m => ServerT RoutesPalantype a m
handlers =
         handleConfigNew

-- handleConfigNew

newtype KeysMapJSON = KeysMapJSON {unKeysMapJSON :: Map RawSteno [Text]}

instance FromJSON KeysMapJSON where
    parseJSON (Array values) =
        let acc :: Map RawSteno [Text] -> Value -> Parser (Map RawSteno [Text])
            acc m a@(Array xs) = do
                (key, value) <- case toList xs of
                    [l1, l2] -> (,) <$> parseJSON l1 <*> parseJSON l2
                    _ -> typeMismatch "values" a
                pure $ Map.insert key value m
            acc _ invalid = typeMismatch "key map" invalid
            map = foldM acc Map.empty values
         in KeysMapJSON <$> map
    parseJSON invalid = typeMismatch "key map" invalid

handleConfigNew :: MonadSnap m => String -> m (SystemLang, PloverSystemCfg)
handleConfigNew str = do
    modifyResponse $ setHeader "Cache-Control" "no-store, must-revalidate"
    let eCfg = runExcept $ do
            parser <- CfgParser.readstring CfgParser.emptyCP str
            let parse = CfgParser.get parser
            systemName <- parse "System" "name"
            machineType <- parse "Machine Configuration" "machine_type"
            unless (machineType == "Keyboard") $
                throwError
                    ( CfgParser.ParseError $ "machine: " <> machineType,
                      "Sorry! Only keyboard is supported."
                    )
            keyMapStr <-
                parse
                    ("System: " <> systemName)
                    ("keymap[" <> machineType <> "]")
            m <-
                case Json.eitherDecode $ LazyBS.fromStrict $ BSU.fromString keyMapStr of
                    Left msg ->
                        throwError
                            ( CfgParser.ParseError msg,
                              "could not decode keymap"
                            )
                    Right km -> pure (km :: KeysMapJSON)
            pure (systemName, machineType, Map.toList $ unKeysMapJSON m)

        errSystemNotImplemented system = err400
            { errBody = LazyBS.fromStrict $ BSU.fromString $ "System "
                          <> system
                          <> " not implemented."
            }
        errNotFound err = err400
          { errBody = LazyBS.fromStrict $ BSU.fromString $ show err
          }

    case eCfg of
        Left err -> Snap.throwError $ errNotFound err
        Right (system, machine, stenoKeys) -> do
            lang <- case system of
                "Palantype"    -> pure SystemEN
                "Palantype DE" -> pure SystemDE
                _              -> Snap.throwError $ errSystemNotImplemented system

            let rawToIndex raw = case lang of
                    SystemEN ->
                        either (const Nothing) (Just . keyIndex) $
                            parseStenoKey @EN.Key raw
                    SystemDE ->
                        either (const Nothing) (Just . keyIndex) $
                            parseStenoKey @DE.Key raw
                acc
                  :: ([(KeyIndex, [Text])], [RawSteno])
                  -> (RawSteno, [Text])
                  -> ([(KeyIndex, [Text])], [RawSteno])
                acc (ls, uSteno) (raw, plovers) = case rawToIndex raw of
                    Just i -> ((i, plovers) : ls, uSteno)
                    Nothing | raw `elem` ["no-op", "arpeggiate"] -> (ls, uSteno)
                    Nothing -> (ls, raw : uSteno)
                (lsIndexPlovers, unrecognizedStenos) =
                    foldl' acc ([], []) stenoKeys

            pure
                ( lang,
                  keyMapToPloverCfg
                      lsIndexPlovers
                      unrecognizedStenos
                      (Text.pack machine)
                      CNFile
                )
