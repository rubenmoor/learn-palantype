{-# LANGUAGE ScopedTypeVariables #-}

module Database where

import           Data.Pool                      ( Pool )
import           Database.Persist.MySQL         ( SqlBackend
                                                , PersistentSqlException
                                                , runSqlPool
                                                )
import           Snap.Core                      ( Snap )
import           Servant.Server                 ( ServantErr(..)
                                                , throwError
                                                , err500
                                                )
import qualified Data.ByteString.Lazy.UTF8     as BSU
import           Control.Monad.Reader           ( asks )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Exception.Lifted              ( catch
                                                , evaluate
                                                )

import           AppData                        ( DbAction
                                                , Handler
                                                , envPool
                                                )

runDb :: DbAction a -> Handler a
runDb action = do
    pool <- asks envPool
    lift $ runDb' pool action

runDb' :: Pool SqlBackend -> DbAction a -> Snap a
runDb' pool action =
    catch (liftIO $ runSqlPool action pool >>= evaluate)
        $ \(e :: PersistentSqlException) -> throwError
              $ err500 { errBody = BSU.fromString $ "db error: " <> show e }
