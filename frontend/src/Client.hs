{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Client where

import           Common.Api                     ( RoutesApi )
import           Common.Auth                    ( CompactJWT
                                                , LoginData(..)
                                                , SessionData(..)
                                                , UserNew(..)
                                                )
import           Common.Model                   ( AppState
                                                , Journal
                                                , Stats
                                                , TextLang, UTCTimeInUrl
                                                )
import           Common.PloverConfig            ( PloverSystemCfg )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( (<<<) )
import           Control.Lens.Getter            ( (^.) )
import           Control.Monad                  ( Monad )
import           Data.Bool                      ( Bool )
import           Data.Either                    ( Either(..)
                                                , either
                                                )
import           Data.Function                  ( ($)
                                                , const
                                                )
import           Data.Functor                   ( Functor(fmap) )
import           Data.Generics.Product          ( field )
import           Data.Int                       ( Int )
import           Data.Maybe                     ( Maybe(..)
                                                , fromMaybe
                                                )
import           Data.Proxy                     ( Proxy(Proxy) )
import           Data.Semigroup                 ( Semigroup((<>)) )
import           Data.String                    ( String )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( decodeUtf8' )
import           Data.Time                      ( Day, UTCTime )
import           Palantype.Common               ( StageIndex
                                                , SystemLang
                                                )
import           Reflex.Dom                     ( Prerender(Client, prerender)
                                                , Reflex(Dynamic, Event, never)
                                                , XhrResponseBody(..)
                                                , constDyn
                                                , switchDyn
                                                , xhrResponse_response
                                                )
import           Servant.API                    ( (:<|>)(..) )
import           Servant.Common.Req             ( ReqResult(..) )
import           Servant.Reflex                 ( BaseUrl(BasePath)
                                                , QParam
                                                , SupportsServantReflex
                                                , client
                                                )
import           State                          ( Session(..)
                                                , State
                                                , stSession
                                                )
import           Text.Pandoc.Definition         ( Pandoc )
import Data.Map.Strict (Map)

postRender :: (Prerender t m, Monad m) => Client m (Event t a) -> m (Event t a)
postRender = fmap switchDyn <<< prerender (pure never)

request
    :: forall t (m :: * -> *) a
     . (Monad m, Prerender t m)
    => Client m (Event t (ReqResult () a))
    -> m (Event t (Either Text a))
request =
  fmap (fmap resultToEither) <<< postRender

resultToEither :: ReqResult () a -> Either Text a
resultToEither = \case
    ResponseSuccess _ x _ -> Right x
    ResponseFailure _ _ resp ->
      let mErrBody = do
              responseBody <- resp ^. xhrResponse_response
              bs           <- case responseBody of
                  XhrResponseBody_ArrayBuffer bs -> pure bs
                  _                              -> Nothing
              either (const Nothing) pure $ decodeUtf8' bs
      in  Left $ fromMaybe "Couldn't decode error body" mErrBody
    RequestFailure _ str -> Left $ "RequestFailure: " <> str

getAuthData
    :: State
    -> Either Text (CompactJWT, Text)
getAuthData st = case stSession st of
    SessionAnon                  -> Left "not logged in"
    SessionUser SessionData {..} -> Right (sdJwt, sdAliasName)

getMaybeAuthData
    :: State -> Either Text (Maybe (CompactJWT, Text))
getMaybeAuthData st =
  Right $ case st ^. field @"stSession" of
    SessionAnon -> Nothing
    SessionUser SessionData{..} -> Just (sdJwt, sdAliasName)

postConfigNew
    :: SupportsServantReflex t m
    => Dynamic t (Either Text String)
    -> Event t ()
    -> m (Event t (ReqResult () (SystemLang, PloverSystemCfg)))

-- auth

postAuthenticate
    :: SupportsServantReflex t m
    => Dynamic t (Either Text LoginData)
    -> Event t ()
    -> m (Event t (ReqResult () (Maybe (SessionData, AppState))))

postAuthNew
    :: SupportsServantReflex t m
    => Dynamic t (Either Text UserNew)
    -> Event t ()
    -> m (Event t (ReqResult () SessionData))

postDoesUserExist
    :: SupportsServantReflex t m
    => Dynamic t (Either Text Text)
    -> Event t ()
    -> m (Event t (ReqResult () Bool))

postDoesAliasExist
    :: SupportsServantReflex t m
    => Dynamic t (Either Text Text)
    -> Event t ()
    -> m (Event t (ReqResult () Bool))

postLogout
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (CompactJWT, Text))
    -> Event t ()
    -> m (Event t (ReqResult () ()))

-- user

postAliasRename
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (CompactJWT, Text))
    -> Dynamic t (Either Text Text)
    -> Event t ()
    -> m (Event t (ReqResult () Text))

getAliasAll
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (CompactJWT, Text))
    -> Event t ()
    -> m (Event t (ReqResult () [Text]))

postAliasSetDefault
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (CompactJWT, Text))
    -> Dynamic t (Either Text Text)
    -> Event t ()
    -> m (Event t (ReqResult () ()))

postAliasVisibility
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (CompactJWT, Text))
    -> Dynamic t (Either Text Bool)
    -> Event t ()
    -> m (Event t (ReqResult () ()))

getAppState
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (CompactJWT, Text))
    -> Event t ()
    -> m (Event t (ReqResult () AppState))

postAppState
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (CompactJWT, Text))
    -> Dynamic t (Either Text AppState)
    -> Event t ()
    -> m (Event t (ReqResult () ()))

postEventViewPage
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (Maybe (CompactJWT, Text)))
    -> Dynamic t (Either Text Text)
    -> Event t ()
    -> m (Event t (ReqResult () ()))

postEventStageCompleted
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (Maybe (CompactJWT, Text)))
    -> Dynamic t (Either Text (SystemLang, StageIndex, Stats))
    -> Event t ()
    -> m (Event t (ReqResult () ()))

-- admin

getJournalAll
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (CompactJWT, Text))
    -> Dynamic t (QParam Day)
    -> Dynamic t (QParam Day)
    -> Dynamic t Bool
    -> Dynamic t (QParam Int)
    -> Dynamic t (QParam Text)
    -> Dynamic t (QParam Text)
    -> Dynamic t Bool
    -> Event t ()
    -> m (Event t (ReqResult () [Journal]))

getLocallyCreateMissingFiles
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (CompactJWT, Text))
    -> Event t ()
    -> m (Event t (ReqResult () ()))

-- stats

getStats
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (Maybe (CompactJWT, Text)))
    -> Dynamic t (Either Text SystemLang)
    -> Dynamic t (Either Text StageIndex)
    -> Event t ()
    -> m (Event t (ReqResult () [(Maybe Text, Stats)]))

postStatsStart
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (CompactJWT, Text))
    -> Event t ()
    -> m (Event t (ReqResult () ()))

-- cms

getCMS
    :: SupportsServantReflex t m
    => Dynamic t (Either Text SystemLang)
    -> Dynamic t (Either Text TextLang)
    -> Dynamic t (Either Text Text)
    -> Dynamic t (Either Text UTCTimeInUrl)
    -> Event t ()
    -> m (Event t (ReqResult () [Pandoc]))

-- | meant to be called from github.com only
--   only implemented here to satisfy the API type
_cmsInvalidateCache
    :: SupportsServantReflex t m
    => Dynamic t (Either Text [Text])
    -> Event t ()
    -> m (Event t (ReqResult () ()))

getCacheInvalidationData
    :: SupportsServantReflex t m
    => Event t ()
    -> m (Event t (ReqResult () (Map (SystemLang, TextLang, Text) UTCTime)))

postClearCacheAll
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (CompactJWT, Text))
    -> Event t ()
    -> m (Event t (ReqResult () ()))

postClearCache
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (CompactJWT, Text))
    -> Dynamic t (Either Text SystemLang)
    -> Dynamic t (Either Text TextLang)
    -> Dynamic t (Either Text Text)
    -> Event t ()
    -> m (Event t (ReqResult () ()))

postCacheUpdateAll
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (CompactJWT, Text))
    -> Event t ()
    -> m (Event t (ReqResult () ()))

getWordList
    :: SupportsServantReflex t m
    => Dynamic t (Either Text Text)
    -> Dynamic t (Either Text Int)
    -> Dynamic t Bool
    -> Event t ()
    -> m (Event t (ReqResult () [Text]))

(postConfigNew :<|> (getJournalAll :<|> getLocallyCreateMissingFiles) :<|> (postAuthenticate :<|> postAuthNew :<|> postDoesUserExist :<|> postDoesAliasExist :<|> postLogout) :<|> ((postAliasRename :<|> getAliasAll :<|> postAliasSetDefault :<|> postAliasVisibility) :<|> (getAppState :<|> postAppState)) :<|> postEventViewPage :<|> (getStats :<|> postStatsStart :<|> postEventStageCompleted) :<|> (getCMS :<|> _cmsInvalidateCache :<|> getCacheInvalidationData :<|> postClearCacheAll :<|> postClearCache :<|> postCacheUpdateAll) :<|> getWordList)
    = client (Proxy :: Proxy RoutesApi)
             (Proxy :: Proxy (m :: * -> *))
             (Proxy :: Proxy ())
             (constDyn (BasePath "/"))
