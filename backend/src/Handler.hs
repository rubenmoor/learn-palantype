{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
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
import           AppData                        ( Handler )

handlers :: ServerT RoutesApi a Handler
handlers =
        Palantype.handlers
   :<|> Admin.handlers
   :<|> Auth.handlers
   :<|> User.handlers
   :<|> Event.handlers
   :<|> Stats.handlers
