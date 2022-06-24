{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Data.Aeson                     ( FromJSON(..)
                                                , Value(Array)
                                                )
import qualified Data.Aeson                    as Json
import           Data.Aeson.Types               ( Parser
                                                , typeMismatch
                                                )
import qualified Data.ByteString.Char8         as Char8
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
import           Data.Maybe                     ( maybe
                                                , Maybe(..)
                                                )
import           Data.Monoid                    ( (<>) )
import           Data.String                    ( String )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           GHC.Show                       ( Show(show) )
import           Obelisk.Generated.Static       ( staticFilePath )
import           Palantype.Common               ( MapStenoWordTake100
                                                , PatternDoc
                                                , KeyIndex
                                                , keyIndex
                                                , Lang(..)
                                                , Greediness
                                                )
import           Palantype.Common               ( RawSteno
                                                , parseStenoKey
                                                , patternDoc
                                                , PatternPos
                                                )
import qualified Palantype.DE.Keys             as DE
import qualified Palantype.EN.Keys             as EN
import           Servant.API                    ( (:<|>)(..) )
import           Servant.Server                 ( err500
                                                , ServantErr(errBody)
                                                , err400
                                                , HasServer(ServerT)
                                                )
import qualified Servant.Server                as Snap
                                                ( throwError )
import           Snap.Core                      ( MonadSnap )
import           Data.Tuple                     ( snd )

handlers :: MonadSnap m => ServerT RoutesPalantype '[] m
handlers =
         handleConfigNew
    :<|> handleDocDEPatternAll
    :<|> handleDocDEPattern
    :<|> handleDictDE
    :<|> handleDictDENumbers

-- handleDocDEPatternAll

getDocMapDE :: MonadSnap m => m (MapStenoWordTake100 DE.Key)
getDocMapDE = do
  let
      errCouldNotDecode = err500
          { errBody = "Could not load map palantype-DE-doc"
          }

  mMap <- liftIO $ Json.decodeFileStrict' $(staticFilePath "palantype-DE-doc.json")
  maybe (Snap.throwError errCouldNotDecode) pure mMap

handleDocDEPatternAll
  :: MonadSnap m => m (PatternDoc DE.Key, MapStenoWordTake100 DE.Key)
handleDocDEPatternAll = do
  map <- getDocMapDE
  pure (patternDoc, map)

-- handleDocDEPattern

handleDocDEPattern
  :: MonadSnap m
  => DE.Pattern
  -> Greediness
  -> m [(PatternPos, [(Text, RawSteno)])]
handleDocDEPattern p g =
  pure $ Map.findWithDefault [] g
           $ Map.fromList
           $ Map.findWithDefault [] p
           $ Map.fromList patternDoc

-- handleDict

handleDictDE :: MonadSnap m => DE.Pattern -> Greediness -> m (Map RawSteno Text, Map Text [RawSteno])
handleDictDE p g = do
  mapPG <- getDocMapDE
  let
      ls = snd $ Map.findWithDefault (0, []) g
               $ Map.findWithDefault Map.empty p mapPG
  pure $
    foldl' (\(mSW, mWSs) (w, s) ->
              ( Map.insert s w mSW
              , Map.insertWith (<>) w [s] mWSs
              )
           ) (Map.empty, Map.empty) ls

handleDictDENumbers :: MonadSnap m => m (Map RawSteno Text)
handleDictDENumbers = do
  let
      errCouldNotDecode = err500
          { errBody = "Could not load map palantype-DE-numbers"
          }

  mMap <- liftIO $ Json.decodeFileStrict'
      $(staticFilePath "palantype-DE-numbers.json")
  maybe (Snap.throwError errCouldNotDecode) pure mMap

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

handleConfigNew :: MonadSnap m => String -> m (Lang, PloverSystemCfg)
handleConfigNew str = do
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
                case Json.eitherDecode $ LazyBS.fromStrict $ Char8.pack keyMapStr of
                    Left msg ->
                        throwError
                            ( CfgParser.ParseError msg,
                              "could not decode keymap"
                            )
                    Right km -> pure (km :: KeysMapJSON)
            pure (systemName, machineType, Map.toList $ unKeysMapJSON m)

        errSystemNotImplemented system = err400
            { errBody = LazyBS.fromStrict $ Char8.pack $ "System "
                          <> system
                          <> " not implemented."
            }
        errNotFound err = err400
          { errBody = LazyBS.fromStrict $ Char8.pack $ show err
          }

    case eCfg of
        Left err -> Snap.throwError $ errNotFound err
        Right (system, machine, stenoKeys) -> do
            lang <- case system of
                "Palantype"    -> pure EN
                "Palantype DE" -> pure DE
                _              -> Snap.throwError $ errSystemNotImplemented system

            let rawToIndex raw = case lang of
                    EN ->
                        either (const Nothing) (Just . keyIndex) $
                            parseStenoKey @EN.Key raw
                    DE ->
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
