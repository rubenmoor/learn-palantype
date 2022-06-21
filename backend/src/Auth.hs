{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Auth
    ( module Auth
    , module Common.Auth
    )
where

import           Data.Pool                      ( Pool )
import           Database.Persist.MySQL         ( SqlBackend
                                                , Entity(..)
                                                )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( Category((.)) )
import           Control.Lens                   ( (?~)
                                                , (^.)
                                                , (^?)
                                                , _Just
                                                )
import           Control.Monad                  ( (>>=) )
import           Control.Monad.Except           ( MonadError
                                                , runExceptT
                                                , throwError
                                                )
import           Control.Monad.Time             ( MonadTime )
import           Crypto.JWT                     ( Audience(..)
                                                , ClaimsSet
                                                , JWK
                                                , JWTError(..)
                                                , MonadRandom
                                                , NumericDate(..)
                                                , StringOrURI
                                                , bestJWSAlg
                                                , claimAud
                                                , claimExp
                                                , claimIat
                                                , claimSub
                                                , decodeCompact
                                                , defaultJWTValidationSettings
                                                , emptyClaimsSet
                                                , encodeCompact
                                                , newJWSHeader
                                                , signClaims
                                                , string
                                                , verifyClaims
                                                )
import           Data.Bool                      ( Bool(..) )
import qualified Data.ByteString.Lazy          as BL
import           Data.Either                    ( either )
import           Data.Eq                        ( Eq((==)) )
import           Data.Function                  ( ($)
                                                , (&)

                                                )
import           Data.Functor                   ( (<$>)
                                                )
import           Data.Maybe                     ( Maybe(..)
                                                , maybe
                                                )
import           Data.String                    ( fromString )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Semigroup                 ( (<>) )
import qualified Data.Text.Encoding            as Text
import           Data.Time                      ( UTCTime
                                                , addUTCTime
                                                )
import           Text.Show                      ( show )
import           Database.Gerippe               ( Key
                                                , InnerJoin(..)
                                                , getWhere
                                                , select
                                                , from
                                                , on
                                                , (==.)
                                                , where_
                                                , val
                                                , (&&.)
                                                , joinMTo1Where'
                                                )
import qualified Database.Gerippe
import           GHC.Generics                   ( Generic )
import qualified Data.ByteString.Lazy.UTF8     as BSU
import           Servant.Server                 ( HasContextEntry
                                                    ( getContextEntry
                                                    )
                                                , HasServer(..)
                                                , Context((:.), EmptyContext)
                                                , ServantErr(..)
                                                , err500
                                                )
import qualified Servant.Server                as Servant
import           Snap.Core                      ( Request
                                                , Snap
                                                , getHeader
                                                , getRequest
                                                )
import           Servant.API                    ( (:>)
                                                , parseHeader
                                                )
import           Data.Proxy                     ( Proxy(Proxy) )
import           Servant.Server.Internal        ( withRequest
                                                , addAuthCheck
                                                , DelayedM
                                                )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Snap.Internal.Core             ( evalSnap )
import           GHC.Base                       ( ($!) )
import           GHC.Exts                       ( seq )
import           GHC.TypeLits                   ( KnownSymbol )

import           AppData                        ( Handler )
import           Common.Auth                    ( AuthProtect
                                                , CompactJWT(CompactJWT)
                                                , SessionData(..)
                                                )
import           Common.Model                   ( Rank(RankOwner) )
import           Database                       ( runDb
                                                , runDb'
                                                )
import           DbAdapter                      ( Alias(..)
                                                , Clearance(..)
                                                , User(..)
                                                , EntityField(..)
                                                )

audience :: StringOrURI
audience = "https://palantype.com"

mkClaims :: UTCTime -> Text -> ClaimsSet
mkClaims now sub =
    emptyClaimsSet
        &  claimAud
        ?~ Audience [audience]
        &  claimExp
        ?~ NumericDate (addUTCTime 30 now)
        &  claimIat
        ?~ NumericDate now
        &  claimSub
        ?~ fromString (Text.unpack sub)

mkCompactJWT
    :: (MonadRandom m, MonadError JWTError m)
    => JWK
    -> ClaimsSet
    -> m CompactJWT
mkCompactJWT jwk claims = do
    alg    <- bestJWSAlg jwk
    signed <- signClaims jwk (newJWSHeader ((), alg)) claims
    pure $ CompactJWT $ Text.decodeUtf8 $ BL.toStrict $ encodeCompact signed

verifyCompactJWT
    :: (MonadError JWTError m, MonadTime m) => JWK -> CompactJWT -> m Text
verifyCompactJWT jwk (CompactJWT str) = do
    jwt <- decodeCompact $ BL.fromStrict $ Text.encodeUtf8 str
    let config = defaultJWTValidationSettings (== audience)
    claims <- verifyClaims config jwk jwt
    case claims ^. claimSub ^? _Just . string of
        Nothing -> throwError $ JWTClaimsSetDecodeError "no subject in claims"
        Just s  -> pure s

data UserInfo = UserInfo
  { uiIsSiteAdmin :: Bool
  , uiUserName    :: Text
  , uiAlias       :: Alias
  , uiKeyAlias    :: Key Alias
  , uiClearances  :: Rank
  } deriving (Generic)

mkSessionData :: CompactJWT -> UserInfo -> SessionData
mkSessionData jwt UserInfo {..} =
    let sdJwt         = jwt
        sdIsSiteAdmin = uiIsSiteAdmin
        sdUserName    = uiUserName
        sdAliasName   = aliasName uiAlias
        sdClearances  = uiClearances
    in  SessionData { .. }

instance ( KnownSymbol tag
         , HasServer api context m
         , HasContextEntry context (Snap UserInfo)
         )
  => HasServer (AuthProtect tag :> api) context m where
    type ServerT (AuthProtect tag :> api) context m
        = UserInfo -> ServerT api context m

    hoistServerWithContext _ pc nt s =
        hoistServerWithContext (Proxy :: Proxy api) pc nt . s

    route (Proxy :: Proxy (AuthProtect tag :> api)) context subserver = route
        (Proxy :: Proxy api)
        context
        (subserver `addAuthCheck` withRequest authCheck)
      where
        authCheck :: Request -> DelayedM m UserInfo
        authCheck = liftIO . evalSnap (getContextEntry context)
                                      (\x -> pure $! (x `seq` ()))
                                      (\f -> let !_ = f 0 in pure ())

getClearances :: Key Alias -> Handler Rank
getClearances keyAlias = do
    Clearance {..} <- runDb (getWhere ClearanceFkAlias keyAlias) >>= \case
        [Entity _ v] -> pure v
        _            -> Servant.throwError
            $ err500 { errBody = "getClearances: expected unique entry" }
    -- TODO
    pure clearanceRank

mkContext :: JWK -> Pool SqlBackend -> Context '[Snap UserInfo]
mkContext jwk pool =
    let
        authHandler :: Snap UserInfo
        authHandler = do
            let toServerError e =
                    Servant.throwError $ err500 { errBody = BSU.fromString e }
            mAuth <- getHeader "Authorization" <$> getRequest
            auth  <- maybe (toServerError "authorization header missing")
                           pure
                           mAuth
            mAlias  <- getHeader "X-Alias" <$> getRequest
            aliasBs <- maybe (toServerError "X-Alias header missing")
                             pure
                             mAlias
            uiAliasName <-
                either
                        (\s -> toServerError $ "parseHeader alias: " <> show s)
                        pure
                    $ parseHeader aliasBs
            jwt <-
                either (\s -> toServerError $ "parseHeader jwt: " <> show s)
                       pure
                    $ parseHeader auth
            eSub       <- liftIO $ runExceptT $ verifyCompactJWT jwk jwt
            uiUserName <- either
                (\s -> toServerError $ "verify jwt: " <> show s)
                pure
                eSub
            ls <- runDb' pool $ select . from $ \(a `InnerJoin` u) -> do
                on
                    $                   a
                    Database.Gerippe.^. AliasFkUser
                    ==.                 u
                    Database.Gerippe.^. UserId
                where_
                    $                   u
                    Database.Gerippe.^. UserName
                    ==.                 val uiUserName
                    &&.                 a
                    Database.Gerippe.^. AliasName
                    ==.                 val uiAliasName
                pure (u, a)
            (Entity _ User {..}, Entity uiKeyAlias uiAlias) <- case ls of
                [entry] -> pure entry
                _ ->
                    toServerError $ "user not found: " <> Text.unpack uiUserName
            let uiIsSiteAdmin = userIsSiteAdmin
            uiClearances <- runDb' pool (getWhere ClearanceFkAlias uiKeyAlias) >>= \case
                [Entity _ Clearance{..}] -> pure clearanceRank
                _                        -> toServerError $ "mkContext: clearance: expect unique entry"
                -- for clearances $ \(Entity _ Clearance{..}, Entity _ Db.Podcast{..}) -> (podcastIdentifier, clearanceRank)
            pure $ UserInfo { .. }
    in
        authHandler :. EmptyContext
