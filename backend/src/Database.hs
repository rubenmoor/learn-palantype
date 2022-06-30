{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Data.Pool                      ( Pool )
import           Database.Persist.MySQL         ( SqlBackend
                                                , PersistentSqlException
                                                , runSqlPool
                                                )
import           Snap.Core                      (MonadSnap )
import           Servant.Server                 ( ServantErr(..)
                                                , throwError
                                                , err500
                                                )
import qualified Data.ByteString.Lazy.UTF8     as BSU
import qualified Data.ByteString.Lazy          as Lazy
import           Control.Monad.Reader           ( asks )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Exception.Lifted       ( catch
                                                , evaluate
                                                )

import           AppData                        ( DbAction
                                                , Handler
                                                , envPool
                                                )
import           Data.Aeson                     (FromJSON,  ToJSON )
import           Data.ByteString.UTF8           ( ByteString )
import qualified Data.Aeson                    as Aeson
import           Control.Category               ( (<<<) )

runDb :: DbAction a -> Handler a
runDb action = do
    pool <- asks envPool
    lift (runDb' pool action) >>= \case
      Left strErr -> throwError $ err500 { errBody = BSU.fromString strErr }
      Right res -> pure res

runDb' :: MonadSnap m => Pool SqlBackend -> DbAction a -> m (Either String a)
runDb' pool action =
    catch (Right <$> liftIO (runSqlPool action pool >>= evaluate))
        $ \(e :: PersistentSqlException) -> pure $ Left $ show e

blobEncode :: forall a . ToJSON a => a -> ByteString
blobEncode = Lazy.toStrict <<< Aeson.encode

blobDecode :: forall a . FromJSON a => ByteString -> Handler a
blobDecode str = case Aeson.eitherDecodeStrict str of
    Left  strErr -> throwError $ err500
      { errBody = "Json decoding failed: " <> BSU.fromString strErr
      }
    Right appState -> pure appState
