{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler
  ( handlers
  ) where

import           Common.Api            (PloverCfg (..), RoutesApi, keyMapToPloverCfg)
import           Control.Applicative   (Applicative (pure, (<*>)))
import           Control.Monad         (foldM, unless)
import           Control.Monad.Except  (MonadError (throwError), runExcept)
import           Data.Aeson            (FromJSON (..), Value (Array))
import qualified Data.Aeson            as Json
import           Data.Aeson.Types      (Parser, typeMismatch)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy  as Lazy
import qualified Data.ConfigFile       as CfgParser
import           Data.Either           (Either (..))
import           Data.Eq               ((==))
import           Data.Foldable         (Foldable (toList))
import           Data.Function         (($))
import           Data.Functor          ((<$>))
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Monoid           ((<>))
import           Data.String           (String)
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           GHC.Show              (Show (show))
import           Servant.Server        (HasServer (ServerT),
                                        ServantErr (errBody), err400)
import qualified Servant.Server        as Snap (throwError)
import           Snap.Core             (Snap)

handlers :: ServerT RoutesApi '[] Snap
handlers = handleConfigNew

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
