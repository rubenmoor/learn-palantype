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
    ( handlers,
    )
where

import Servant.API ((:<|>) (..))
import Servant.Server
    (HasServer (ServerT)
    )
import Snap.Core (Snap)

import Common.Api
    ( RoutesApi,
    )
import qualified Handler.Palantype as Palantype
import qualified Handler.Auth as Auth
import qualified Handler.User as User
import Auth (UserInfo)
import AppData (Handler)

handlers :: ServerT RoutesApi '[] Handler
handlers =
       Palantype.handlers
  :<|> Auth.handlers
  :<|> User.handlers
