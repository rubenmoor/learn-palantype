{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler
    ( handlers
    ) where

import           Common.Api                     ( Lang(..)
                                                , PloverSystemCfg
                                                , Routes
                                                , keyMapToPloverCfg
                                                )
import           Control.Applicative            ( Applicative((<*>), pure) )
import           Control.Category               ( Category((.)) )
import           Control.Monad                  ( foldM
                                                , unless
                                                )
import           Control.Monad                  ( Monad((>>=)) )
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
import qualified Data.ByteString.Lazy          as Lazy
import qualified Data.ConfigFile               as CfgParser
import           Data.Either                    ( Either(..)
                                                , either
                                                )
import           Data.Eq                        ( (==) )
import           Data.FileEmbed                 ( embedFile
                                                , makeRelativeToProject
                                                )
import           Data.Foldable                  ( Foldable(elem, toList)
                                                , foldl'
                                                )
import           Data.Function                  ( ($)
                                                , const
                                                )
import           Data.Functor                   ( (<$>) )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.List                      ( (++) )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( Maybe(..) )
import           Data.Monoid                    ( (<>) )
import           Data.String                    ( String )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           GHC.Show                       ( Show(show) )
import           Palantype.Common               ( KeyIndex
                                                , Palantype(keyIndex)
                                                )
import           Palantype.Common.RawSteno      ( RawSteno
                                                , parseStenoKey
                                                )
import qualified Palantype.DE.Keys             as DE
import qualified Palantype.EN.Keys             as EN
import           Servant.API                    ( (:<|>)(..) )
import           Servant.Server                 ( HasServer(ServerT)
                                                , ServantErr(errBody)
                                                , err400
                                                , err500
                                                )
import qualified Servant.Server                as Snap
                                                ( throwError )
import           Snap.Core                      ( Snap )

handlers :: ServerT Routes '[] Snap
handlers = handleConfigNew :<|> handleDictTop2k

-- handleDictTop2k

handleDictTop2k :: Snap (HashMap RawSteno Text, HashMap Text [RawSteno])
handleDictTop2k = do
    let str = $(makeRelativeToProject "top2k.json" >>= embedFile)
    mStenoWord <- case Json.eitherDecodeStrict str of
        Right m   -> pure m
        Left  err -> Snap.throwError $ err500
            { errBody = "Could not decode top2k.json: "
                            <> Lazy.fromStrict (Char8.pack $ show err)
            }
    let acc m (raw, word) = HashMap.insertWith (++) word [raw] m
        mWordStenos = foldl' acc HashMap.empty $ HashMap.toList mStenoWord
    pure (mStenoWord, mWordStenos)

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
                    Json.eitherDecode $ Lazy.fromStrict $ Char8.pack keyMapStr
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
            $ err400 { errBody = Lazy.fromStrict $ Char8.pack $ show err }
        Right (system, machine, stenoKeys) -> do

            lang <- case system of
                "Palantype"    -> pure EN
                "Palantype DE" -> pure DE
                _              -> Snap.throwError $ err400
                    { errBody = Lazy.fromStrict
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

            let acc
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
                )
