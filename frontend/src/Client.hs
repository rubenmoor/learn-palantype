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


import           Common.PloverConfig            ( PloverSystemCfg )
import           Common.Api                     (RoutesApi )
import           Common.Auth                    ( CompactJWT
                                                , SessionData(..)
                                                , LoginData(..)
                                                , UserNew(..)
                                                )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Monad                  ( Monad )
import           Data.Generics.Product          ( field )
import           Data.Either                    ( either
                                                , Either(..)
                                                )
import           Data.Proxy                     ( Proxy(Proxy) )
import           Data.String                    ( String )
import           Data.Text                      ( Text )
import           Reflex.Dom                     ( XhrResponseBody(..)
                                                , xhrResponse_response
                                                , Prerender(Client, prerender)
                                                , Reflex(Dynamic, Event, never)
                                                , constDyn
                                                , switchDyn
                                                )
import           Servant.Common.Req             ( ReqResult(..) )
import           Servant.Reflex                 ( BaseUrl(BasePath)
                                                , SupportsServantReflex
                                                , client
                                                )
import           Data.Maybe                     ( fromMaybe
                                                , Maybe(..)
                                                )
import           Data.Function                  ( const
                                                , ($)
                                                )
import           Data.Semigroup                 ( Semigroup((<>)) )
import           Servant.API                    ( (:<|>)(..) )
import           Palantype.Common               ( Lang
                                                )
import           State                          ( stSession
                                                , State
                                                , Session(..)
                                                )
import           Data.Bool                      ( Bool )
import           Control.Lens.Getter            ( (^.) )
import           Data.Text.Encoding             ( decodeUtf8' )
import           Common.Model                   ( Journal
                                                , Stats
                                                , AppState
                                                )
import           Common.Stage                   (StageIndex,  Stage )
import           Data.Functor                   ( Functor(fmap) )
import           Data.Time                      ( Day )
import Servant.Reflex (QParam)
import Data.Int (Int)
import Control.Category ((<<<))

postRender :: (Prerender t m, Monad m) => Client m (Event t a) -> m (Event t a)
postRender = fmap switchDyn <<< prerender (pure never)

request
    :: forall t (m :: * -> *) a
     . (Monad m, Prerender t m)
    => Client m (Event t (ReqResult () a))
    -> m (Event t (Either Text a))
request =
    fmap (fmap resultToEither <<< switchDyn) <<< prerender (pure never)

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
    -> (Either Text (CompactJWT, Text))
getAuthData st = case stSession st of
    SessionAnon                  -> Left "not logged in"
    SessionUser SessionData {..} -> Right (sdJwt, sdAliasName)

getMaybeAuthData
    :: State -> (Either Text (Maybe (CompactJWT, Text)))
getMaybeAuthData st =
  Right $ case st ^. field @"stSession" of
    SessionAnon -> Nothing
    SessionUser SessionData{..} -> Just (sdJwt, sdAliasName)

postConfigNew
    :: SupportsServantReflex t m
    => Dynamic t (Either Text String)
    -> Event t ()
    -> m (Event t (ReqResult () (Lang, PloverSystemCfg)))

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
    -> m (Event t (ReqResult () (AppState)))

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
    -> Dynamic t (Either Text (Lang, StageIndex, Stats))
    -> Event t ()
    -> m (Event t (ReqResult () ()))

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

getStats
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (Maybe (CompactJWT, Text)))
    -> Dynamic t (Either Text Lang)
    -> Dynamic t (Either Text StageIndex)
    -> Event t ()
    -> m (Event t (ReqResult () [(Maybe Text, Stats)]))

postStatsStart
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (CompactJWT, Text))
    -> Event t ()
    -> m (Event t (ReqResult () ()))

((postConfigNew) :<|> (getJournalAll) :<|> (postAuthenticate :<|> postAuthNew :<|> postDoesUserExist :<|> postDoesAliasExist :<|> postLogout) :<|> ((postAliasRename :<|> getAliasAll :<|> postAliasSetDefault :<|> postAliasVisibility) :<|> (getAppState :<|> postAppState)) :<|> (postEventViewPage) :<|> (getStats :<|> postStatsStart :<|> postEventStageCompleted))
    = client (Proxy :: Proxy RoutesApi)
             (Proxy :: Proxy (m :: * -> *))
             (Proxy :: Proxy ())
             (constDyn (BasePath "/"))
