{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DataKinds #-}
module Backend where

import Common.Route
    ( FrontendRoute, BackendRoute, fullRouteEncoder )
import Obelisk.Backend ( Backend(..) )
import Snap.Core (Snap)
import Data.Typeable (Proxy (..))
import Common.Api ( RoutesApi, PloverCfg(..) )
import "servant-snap" Servant.Server (HasServer(ServerT), serveSnap)
import Servant.Multipart (Mem, MultipartData)

serveApi :: Snap ()
serveApi = serveSnap (Proxy :: Proxy RoutesApi) handlers

handlers :: ServerT RoutesApi '[] Snap
handlers = handleConfigNew

handleConfigNew :: MultipartData Mem -> Snap PloverCfg
handleConfigNew tmp = pure PloverCfg

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ const $ return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
