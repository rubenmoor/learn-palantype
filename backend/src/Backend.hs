{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}

module Backend where

import           Common.Api                     ( Routes
                                                , RoutesApi
                                                )
import           Common.Route                   ( BackendRoute
                                                    ( BackendRoute_Api
                                                    , BackendRoute_Missing
                                                    )
                                                , FrontendRoute
                                                , fullRouteEncoder
                                                )
import           Data.Typeable                  ( Proxy(..) )
import           Handler                        ( handlers )
import           Obelisk.Backend                ( Backend(..) )
import           Obelisk.Route                  ( pattern (:/) )
import           Servant.Server                 ( Context(EmptyContext)
                                                , serveSnapWithContext
                                                )
import           Snap.Core                      ( Snap )

serveApi :: Snap ()
serveApi = serveSnapWithContext (Proxy :: Proxy Routes) EmptyContext handlers

backend :: Backend BackendRoute FrontendRoute
backend = Backend
    { _backend_run          = \serve -> serve $ \case
                                  (BackendRoute_Missing :/ _) -> pure ()
                                  (BackendRoute_Api     :/ _) -> serveApi
    , _backend_routeEncoder = fullRouteEncoder
    }
