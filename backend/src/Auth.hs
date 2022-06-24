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
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Auth
    ( module Auth
    , module Common.Auth
    )
where

import qualified Data.ByteString.Lazy.UTF8 as LBSU
import           Data.Pool                      ( Pool )
import           Database.Persist.MySQL         ( SqlBackend
                                                , Entity(..)
                                                )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ((<<<),  Category((.)) )
import           Control.Lens                   ( (?~)
                                                , (^.)
                                                , (^?)
                                                , _Just
                                                )
import           Control.Monad                  ( (>>=) )
import           Control.Monad.Except           (lift,  MonadError
                                                , runExceptT
                                                , throwError
                                                )
import           Crypto.JWT                     (verifyClaimsAt,  Audience(..)
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

                                                )
import           Data.Bool                      ( Bool(..) )
import qualified Data.ByteString.Lazy          as BL
import           Data.Either                    ( Either (..), either )
import           Data.Eq                        ( Eq((==)) )
import           Data.Function                  ( ($)
                                                , (&)

                                                )
import           Data.Functor                   ((<$>)
                                                )
import           Data.Maybe                     (Maybe(..)
                                                , maybe
                                                )
import           Data.String                    ( fromString )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Semigroup                 ( (<>) )
import qualified Data.Text.Encoding            as Text
import           Data.Time                      (getCurrentTime,  UTCTime
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

                                                )
import qualified Database.Gerippe
import           GHC.Generics                   ( Generic )
import           Servant.Server                 ( HasContextEntry
                                                    ( getContextEntry
                                                    )
                                                , HasServer(..)
                                                , Context((:.), EmptyContext)
                                                , ServantErr(..)
                                                , err500
                                                )
import qualified Servant.Server                as Servant
import           Snap.Core                      ( Snap
                                                , getHeader
                                                , getRequest
                                                )
import           Servant.API                    ( (:>)
                                                , parseHeader
                                                )
import           Data.Proxy                     ( Proxy(Proxy) )
import           Servant.Server.Internal        (delayedFailFatal,  withRequest
                                                , addAuthCheck
                                                , DelayedM
                                                )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Snap.Internal.Core             ( evalSnap )
import           GHC.Base                       (($!) )
import           GHC.Exts                       ( seq )
import           GHC.TypeLits                   ( KnownSymbol )

import           AppData                        ( Handler )
import           Common.Auth                    (AuthOptional, AuthError (..),  AuthRequired
                                                , CompactJWT(CompactJWT)
                                                , SessionData(..)
                                                )
import           Common.Model                   ( Rank )
import           Database                       ( runDb
                                                , runDb'
                                                )
import qualified          DbAdapter                      as Db
import Snap.Core (MonadSnap)
import GHC.Num (Num((*)))
import Data.Either.Combinators (mapLeft)
import Control.Monad.Trans.Except (except)
import Servant.Server (err401)
import Control.Monad ((=<<))
import Data.Function (const)

audience :: StringOrURI
audience = "https://palantype.com"

mkClaims :: UTCTime -> Text -> ClaimsSet
mkClaims now sub =
    emptyClaimsSet
        &  claimAud ?~ Audience [audience]
        &  claimExp ?~ NumericDate (addUTCTime (30 * 24 * 60 * 60) now)
        &  claimIat ?~ NumericDate now
        &  claimSub ?~ fromString (Text.unpack sub)

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
    -- :: forall m. (MonadSnap m, MonadError JWTError m) => JWK -> CompactJWT -> UTCTime -> m Text
    :: forall m. MonadError JWTError m => JWK -> CompactJWT -> UTCTime -> m Text
verifyCompactJWT jwk (CompactJWT str) now = do
    jwt <- decodeCompact $ BL.fromStrict $ Text.encodeUtf8 str
    let config = defaultJWTValidationSettings (== audience)
    claims <- verifyClaimsAt config jwk now jwt
    case claims ^. claimSub ^? _Just . string of
            Nothing -> throwError $ JWTClaimsSetDecodeError "no subject in claims"
            Just sub -> pure sub

data UserInfo = UserInfo
  { uiIsSiteAdmin :: Bool
  , uiUserName    :: Text
  , uiAlias       :: Db.Alias
  , uiKeyAlias    :: Key Db.Alias
  , uiClearances  :: Rank
  } deriving (Generic)

mkSessionData :: CompactJWT -> UserInfo -> SessionData
mkSessionData jwt UserInfo {..} =
    let sdJwt         = jwt
        sdIsSiteAdmin = uiIsSiteAdmin
        sdUserName    = uiUserName
        sdAliasName   = Db.aliasName uiAlias
        sdClearances  = uiClearances
    in  SessionData { .. }

type ContextAuth = Snap (Either AuthError UserInfo)

instance ( KnownSymbol tag
         , HasServer api context m
         , HasContextEntry context (Snap (Either AuthError UserInfo))
         )
  => HasServer (AuthRequired tag :> api) context m where
    type ServerT (AuthRequired tag :> api) context m
        = UserInfo -> ServerT api context m

    hoistServerWithContext _ pc nt s =
        hoistServerWithContext (Proxy :: Proxy api) pc nt . s

    route (Proxy :: Proxy (AuthRequired tag :> api)) context subserver = route
        (Proxy :: Proxy api)
        context
        (addAuthCheck subserver $ authRequire =<< authCheck (getContextEntry context))
      where
        authRequire = either
          (\err -> delayedFailFatal $ err401
            { errHeaders = [("WWW-Authenticate", "Bearer")]
            , errBody = LBSU.fromString $ show err
            }
          )
          pure

authCheck :: MonadIO m => ContextAuth -> DelayedM m (Either AuthError UserInfo)
authCheck a = withRequest $ liftIO <<< evalSnap
    a
    (\x -> pure $! (x `seq` ()))
    (\f -> let !_ = f 0 in pure ())

instance ( KnownSymbol tag
         , HasServer api context m
         , HasContextEntry context (Snap (Either AuthError UserInfo))
         )
  => HasServer (AuthOptional tag :> api) context m where
    type ServerT (AuthOptional tag :> api) context m
        = Maybe UserInfo -> ServerT api context m

    hoistServerWithContext _ pc nt s =
        hoistServerWithContext (Proxy :: Proxy api) pc nt . s

    route (Proxy :: Proxy (AuthOptional tag :> api)) context subserver = route
        (Proxy :: Proxy api)
        context
        (addAuthCheck subserver $ authOptional <$> authCheck (getContextEntry context))
       where
         authOptional = either (const Nothing) Just


getClearances :: Key Db.Alias -> Handler Rank
getClearances keyAlias = do
    Db.Clearance {..} <- runDb (getWhere Db.ClearanceFkAlias keyAlias) >>= \case
        [Entity _ v] -> pure v
        []           -> Servant.throwError $
            err500 { errBody = "getClearances: empty list" }
        _            -> Servant.throwError $
            err500 { errBody = "getClearances: expected single entry" }
    pure clearanceRank

mkContext :: JWK -> Pool SqlBackend -> Context '[Snap (Either AuthError UserInfo)]
mkContext jwk pool =
    let
        authHandler :: MonadSnap m => m (Either AuthError UserInfo)
        authHandler = runExceptT do
            auth <- lift (getHeader "Authorization" <$> getRequest) >>=
              maybe (throwError $ AuthErrorOther "authorization header missing")
                    pure
            aliasBs <- lift (getHeader "X-Alias" <$> getRequest) >>=
              maybe (throwError $ AuthErrorOther "X-Alias header missing")
                    pure
            uiAliasName <-
              except $ mapLeft (AuthErrorOther <<< ("parseHeader alias: " <>)) $
                parseHeader aliasBs
            jwt <-
              either (throwError <<< AuthErrorOther <<< ("parseHeader jwt: " <>))
                     pure
                     (parseHeader auth)

            now <- liftIO $ getCurrentTime
            eSub <- runExceptT $ verifyCompactJWT jwk jwt now
            uiUserName <-
              either
                (\case
                    JWTExpired -> throwError AuthErrorExpired
                    e          -> throwError $ AuthErrorOther $ Text.pack $ show e
                    )
                pure
                eSub
            ls <- lift $ runDb' pool $ select $ from $ \(a `InnerJoin` u) -> do
                on $ a Database.Gerippe.^. Db.AliasFkUser ==. u Database.Gerippe.^. Db.UserId
                where_ $ u Database.Gerippe.^. Db.UserName ==. val uiUserName
                  &&. a Database.Gerippe.^. Db.AliasName   ==. val uiAliasName
                pure (u, a)
            (Entity _ Db.User {..}, Entity uiKeyAlias uiAlias) <- case ls of
                [entry] -> pure entry
                _       -> throwError $ AuthErrorOther $ "user not found: " <> uiUserName
            let uiIsSiteAdmin = userIsSiteAdmin
            uiClearances <- lift (runDb' pool $ getWhere Db.ClearanceFkAlias uiKeyAlias) >>= \case
                [Entity _ Db.Clearance{..}] -> pure clearanceRank
                _                        -> throwError $ AuthErrorOther $ "mkContext: clearance: expect unique entry"
                -- for clearances $ \(Entity _ Clearance{..}, Entity _ Db.Podcast{..}) -> (podcastIdentifier, clearanceRank)
            pure $ UserInfo { .. }
    in
        authHandler :. EmptyContext
