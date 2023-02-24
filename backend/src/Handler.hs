{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler
    ( handlers
    )
where

import           Servant.API                    ( (:<|>)(..) )
import           Servant.Server                 ( HasServer(ServerT) )

import           Common.Api                     ( RoutesApi )
import qualified Handler.Palantype             as Palantype
import qualified Handler.Auth                  as Auth
import qualified Handler.User                  as User
import qualified Handler.Event                 as Event
import qualified Handler.Admin                 as Admin
import qualified Handler.Stats                 as Stats
import qualified Handler.CMS                   as CMS
import           AppData                        ( Handler )

handlers :: ServerT RoutesApi a Handler
handlers =
        Palantype.handlers
   :<|> Admin.handlers
   :<|> Auth.handlers
   :<|> User.handlers
   :<|> Event.handlers
   :<|> Stats.handlers
   :<|> CMS.handlers
