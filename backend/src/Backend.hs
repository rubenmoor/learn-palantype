{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}

module Backend where

import           Common.Api      (RoutesApi)
import           Common.Route    (BackendRoute (BackendRoute_Api, BackendRoute_Missing),
                                  FrontendRoute, fullRouteEncoder)
import           Data.Typeable   (Proxy (..))
import           Obelisk.Backend (Backend (..))
import           Obelisk.Route   (pattern (:/))
import           Servant.Server  (Context (EmptyContext),
                                  serveSnapWithContext)
import           Snap.Core       (Snap)
import Handler (handlers)

serveApi :: Snap ()
serveApi = serveSnapWithContext (Proxy :: Proxy RoutesApi) EmptyContext handlers

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ \case
      (BackendRoute_Missing :/ _) -> pure ()
      (BackendRoute_Api :/ _) -> serveApi
  , _backend_routeEncoder = fullRouteEncoder
  }
