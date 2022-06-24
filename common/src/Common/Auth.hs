{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Auth
  ( AuthError (..)
  , CompactJWT (..)
  , LoginData (..)
  , UserNew (..)
  , AuthRequired
  , AuthOptional
  , SessionData (..)
  ) where

import           Control.Arrow           (ArrowChoice ((+++)))
import           Data.Aeson              (FromJSON, ToJSON)
import           Data.Maybe              (fromMaybe)
import           Data.Proxy              (Proxy (Proxy))
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import           Data.Typeable           (Typeable)
import           GHC.Generics            (Generic)
import           Common.Model            (AppState, Rank)
import           Reflex.Dom              (splitDynPure, Reflex (Dynamic))
import           Servant.API             ((:>))
import           Servant.Common.Req      (addHeader)
import           Servant.Reflex          (HasClient (..))
import           Web.HttpApiData         (FromHttpApiData (..),
                                          ToHttpApiData (..))

newtype CompactJWT = CompactJWT
  { unCompactJWT :: Text
  } deriving (Eq, Show, FromJSON, ToJSON)

instance (FromHttpApiData CompactJWT) where
    parseQueryParam = Right . CompactJWT
    parseHeader =
      let toCompactJWT str =
            CompactJWT $ fromMaybe str $ Text.stripPrefix "Bearer " str
      in  (Text.pack . show +++ toCompactJWT) . Text.decodeUtf8'
instance (ToHttpApiData CompactJWT) where
    toQueryParam (CompactJWT t) = t
    toHeader (CompactJWT t) = "Bearer " <> Text.encodeUtf8 t

data LoginData = LoginData
  { ldUserName :: Text
  , ldPassword :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON LoginData
instance ToJSON LoginData

data UserNew = UserNew
  { unUserName :: Text
  , unPassword :: Text
  , unMAlias   :: Maybe Text
  , unVisible  :: Bool
  , unAppState :: AppState
  } deriving (Eq, Show, Generic)

instance FromJSON UserNew
instance ToJSON UserNew

-- Servant API

-- combinator for protected route

data AuthRequired (tag :: k) deriving (Typeable)

instance (HasClient t m api tag, Reflex t)
      => HasClient t m (AuthRequired realm :> api) tag where

  type Client t m (AuthRequired realm :> api) tag =
       Dynamic t (Either Text (CompactJWT, Text))
    -> Client t m api tag

  clientWithRouteAndResultHandler Proxy q t req baseurl opts wrap authData =
    clientWithRouteAndResultHandler (Proxy :: Proxy api) q t req' baseurl opts wrap
      where
        switchEither (Left str) = (Left str, Left str)
        switchEither (Right (x, y)) = (Right x, Right y)
        (token, alias) = splitDynPure $ switchEither <$> authData
        req' = addHeader "Authorization" token $
               addHeader "X-Alias" alias req

-- combinator for a route that accepts both, anonymous or authenticated requests

data AuthOptional (tag :: k) deriving (Typeable)

instance (HasClient t m api tag, Reflex t)
      => HasClient t m (AuthOptional realm :> api) tag where

  type Client t m (AuthOptional realm :> api) tag =
       Maybe (Dynamic t (Either Text (CompactJWT, Text)))
    -> Client t m api tag

  clientWithRouteAndResultHandler Proxy q t req baseurl opts wrap mAuthData =
    clientWithRouteAndResultHandler (Proxy :: Proxy api) q t req' baseurl opts wrap
      where
        switchEither (Left str) = (Left str, Left str)
        switchEither (Right (x, y)) = (Right x, Right y)
        req' = case mAuthData of
          Nothing -> req
          Just authData ->
            let (token, alias) = splitDynPure $ switchEither <$> authData
            in  addHeader "Authorization" token $ addHeader "X-Alias" alias req



data SessionData = SessionData
  { sdJwt :: CompactJWT
  , sdIsSiteAdmin :: Bool
  , sdUserName :: Text
  , sdAliasName :: Text
  , sdClearances :: Rank
  } deriving (Generic)

instance ToJSON SessionData
instance FromJSON SessionData

data AuthError
  = AuthErrorExpired
  | AuthErrorOther Text

instance Show AuthError where
  show = \case
    AuthErrorExpired   -> "Session Expired"
    AuthErrorOther str -> Text.unpack str
