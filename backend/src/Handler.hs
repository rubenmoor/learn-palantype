{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler
  ( handlers
  ) where

import Servant.Server (err400, ServantErr(errBody), HasServer(ServerT))
import qualified Servant.Server as Snap (throwError)
import Snap.Core (Snap)
import Common.Api (PloverCfg(..), RoutesApi)
import Data.Text ( Text )
import Control.Applicative (Applicative((<*>), pure))
import Data.String (String)
import qualified Data.ConfigFile as CfgParser
import Data.Function (flip, ($))
import Control.Monad.Except (MonadError(throwError), runExcept)
import Data.List (lookup)
import Control.Monad (foldM, unless, when)
import Data.Monoid ((<>))
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Char8 as Char8
import Data.Either (Either(..))
import Data.Traversable (for)
import Data.Functor
import Data.Aeson (Value (Array), FromJSON (..))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Foldable (Foldable(toList, foldl))
import qualified Data.Text as Text
import GHC.Show (Show(show))

handlers :: ServerT RoutesApi '[] Snap
handlers = handleConfigNew

newtype KeysMap = KeysMap { unKeysMap :: Map Text [Text]}

instance FromJSON KeysMap where
  parseJSON (Array values) =
    let acc :: Map Text [Text] -> Value -> Parser (Map Text [Text])
        acc m a@(Array xs) = do
          (key, value) <- case toList xs of
            [l1, l2] -> (,) <$> parseJSON l1 <*> parseJSON l2
            _        -> typeMismatch "values" a
          pure $ Map.insert key value m
        acc _ invalid = typeMismatch "key map" invalid
        map = foldM acc Map.empty values
    in  KeysMap <$> map
  parseJSON invalid = typeMismatch "key map" invalid

handleConfigNew :: String -> Snap PloverCfg
handleConfigNew str =
  let eCfg = runExcept $ do

        -- throw msg = throwError
        parser <- CfgParser.readstring CfgParser.emptyCP str
        let parse = CfgParser.get parser
        systemName <- parse "System" "name"
        machineType <- parse "Machine Configuration" "machine_type"
        keyMapStr <- parse ("System: " <> systemName) ("keymap[" <> machineType <> "]")
        keysMap <- case Json.eitherDecode $ Lazy.fromStrict $ Char8.pack keyMapStr of
          Left msg -> throwError (CfgParser.ParseError msg, "could not decode keymap")
          Right km -> pure (km :: KeysMap)

        let keyStenoMap =
              let acc :: Map Text Text -> (Text, [Text]) -> Map Text Text
                  acc m (key, values) = foldl (\m' value -> Map.insert value key m') m values
              in  foldl acc Map.empty $ Map.toList $ unKeysMap keysMap

        pure $ PloverCfg
          { pcfgStenoKeys = unKeysMap keysMap
          , pcfgKeySteno = keyStenoMap
          , pcfgSystem = Text.pack systemName
          , pcfgMachine = Text.pack machineType
          }
  in  case eCfg of
        Left err -> Snap.throwError $
          err400 { errBody = Lazy.fromStrict  $ Char8.pack $ show err }
        Right cfg -> pure cfg
