{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend where

import           Common.Route                   ( BackendRoute
                                                    ( BackendRoute_Api
                                                    , BackendRoute_Missing
                                                    )
                                                , FrontendRoute
                                                , fullRouteEncoder
                                                )
import           Data.Typeable                  ( Proxy(..) )
import           Database.Persist.MySQL         ( ConnectInfo(..)
                                                , defaultConnectInfo
                                                , withMySQLPool
                                                , runSqlPool
                                                , runMigration
                                                )
import           Database.Persist.TH            ( migrateModels )
import           Database.MySQL.Base.Types      ( Option(CharsetName) )
import           Handler                        ( handlers )
import           Obelisk.Backend                ( Backend(..) )
import           Obelisk.Route                  ( pattern (:/) )
import           Obelisk.Configs                ( ConfigsT
                                                , runConfigsT
                                                , getTextConfig
                                                , HasConfigs(getConfig)
                                                )
import           Obelisk.ExecutableConfig.Lookup
                                                ( getConfigs )
import           Servant.Server                 ( Context
                                                , serveSnapWithContext
                                                )
import           Snap.Core                      ( Snap )
import           Data.Aeson                     ( FromJSON )
import qualified Data.Aeson                    as Aeson
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Reader           ( runReaderT )
import           Control.Monad.Logger           ( NoLoggingT(..) )
import           Control.Monad.Trans.Resource   ( runResourceT )
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as Text
import           System.Exit                    ( exitWith
                                                , ExitCode(ExitFailure)
                                                )

import           Config                         ( Params(..) )
import           Common.Api                     ( RoutesApi )
import           Auth                           (AuthError,  UserInfo
                                                , mkContext
                                                )
import           AppData                        ( EnvApplication(..) )
import           DbAdapter                      ( entities )

serveApi :: Context '[Snap (Either AuthError UserInfo)] -> EnvApplication -> Snap ()
serveApi ctx =
    runReaderT $ serveSnapWithContext (Proxy :: Proxy RoutesApi) ctx handlers

backend :: Backend BackendRoute FrontendRoute
backend = Backend
    { _backend_run          = \serve -> do
        cfgs                    <- getConfigs
        (Params {..}, jwk, url) <- runConfigsT cfgs $ do
            params <- getConfigOrExit "backend/params"
            jwk    <- getConfigOrExit "backend/jwk"
            mUrl   <- getTextConfig "common/route"
            url    <- maybe (exitWithFailure "config common/route not found")
                            pure
                            mUrl
            pure (params, jwk, url)
        let connectInfo = defaultConnectInfo
                { connectHost     = paramDbHost
                , connectUser     = paramDbUser
                , connectPassword = paramDbPassword
                , connectDatabase = paramDbDatabase
                , connectOptions  = [CharsetName "utf8mb4"]
                }
        runNoLoggingT $ withMySQLPool connectInfo 10 $ \pool -> do
            runResourceT
                $ runSqlPool (runMigration $ migrateModels entities) pool
            let env = EnvApplication { envPool     = pool
                                     , envUrl      = url
                                     , envJwk      = jwk
                                     }
                ctx = mkContext jwk pool
            NoLoggingT $ serve $ \case
                (BackendRoute_Missing :/ _) -> pure ()
                (BackendRoute_Api     :/ _) -> serveApi ctx env
    , _backend_routeEncoder = fullRouteEncoder
    }

getConfigOrExit :: forall a m . (MonadIO m, FromJSON a) => Text -> ConfigsT m a
getConfigOrExit filename = do
    mStr <- getConfig filename
    str  <- maybe (exitWithFailure $ "config " <> filename <> " not found")
                  pure
                  mStr
    maybe (exitWithFailure $ "could not decode " <> filename) pure
        $ Aeson.decodeStrict str

exitWithFailure :: MonadIO m => Text -> m a
exitWithFailure msg = liftIO $ do
    Text.putStrLn msg
    exitWith $ ExitFailure 1
