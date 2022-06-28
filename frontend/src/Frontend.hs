{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Frontend where

import           Control.Lens.Getter            ( (^.) )
import           Control.Lens.Setter            ( (?~), (.~))
import           Data.Generics.Product          ( field )
import           Language.Javascript.JSaddle    ( liftJSM )
import           State                          (updateState,  Session(..)
                                                , State(..)
                                                , defaultState
                                                , stageUrl
                                                )

import           Common.Route                   (showRoute, fullRouteEncoder,  FrontendRoute(..)
                                                , FrontendRoute_AuthPages(..)
                                                )
import           Control.Monad.Reader           (MonadReader, ask,  ReaderT(runReaderT) )
import qualified Data.Aeson                    as Aeson
import           Data.Functor                   ( ($>)
                                                , (<&>)
                                                )
import           Data.Function                  ( (.), (&) )
import qualified Data.Map                      as Map
import           Data.Maybe                     (maybe, isJust, isNothing,  Maybe(..)
                                                , fromMaybe
                                                )
import           Data.Semigroup                 ( (<>)
                                                , Endo(..)
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text.Lazy                as Lazy
import qualified Data.Text.Lazy.Encoding       as Lazy
import           GHCJS.DOM                      ( currentWindowUnchecked )
import           GHCJS.DOM.Storage              ( getItem
                                                , setItem
                                                )
import           GHCJS.DOM.Window               (getLocalStorage)
import           Home                           ( message

                                                , landingPage
                                                , stages
                                                )
import           Obelisk.Frontend               ( Frontend(..)
                                                , ObeliskWidget
                                                )
import           Obelisk.Generated.Static       ( static )
import           Obelisk.Route                  ( pattern (:/)
                                                , R
                                                )
import           Obelisk.Route.Frontend         (Routed, askRoute,  RoutedT
                                                , SetRoute(setRoute)
                                                , mapRoutedT
                                                , subRoute_
                                                )
import           Palantype.Common               ( Lang(..) )
import qualified Palantype.DE.Keys             as DE
import qualified Palantype.EN.Keys             as EN
import           Reflex.Dom                     (Prerender, fanEither, attachPromptlyDynWith, select, fanMap, attachWith, attachWithMaybe, gate, widgetHold_, delay, performEvent, current, attach, Dynamic, never, switchDyn, constDyn
                                                , holdDyn

                                                , prerender


                                                , PerformEvent(performEvent_)
                                                , PostBuild(getPostBuild)
                                                , Reflex(updated)
                                                , blank
                                                , dyn_
                                                , el
                                                , elAttr
                                                , foldDyn
                                                , leftmost
                                                , prerender_
                                                , runEventWriterT
                                                , tailE
                                                , text
                                                , (=:)
                                                )

import           Shared                         (requestPostViewPage, redirectToWikipedia,  loadingScreen )
import qualified AuthPages
import           Control.Category               ( (<<<))
import           Common.Model                   (Rank (RankAdmin),  Message(..)
                                                , defaultAppState
                                                )
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           Common.Auth                    ( SessionData(..) )
import           Client                         (getMaybeAuthData, postEventViewPage,  postAppState
                                                , request
                                                , getAppState
                                                )
import           Data.Either                    ( either
                                                , Either(..)
                                                )
import           Control.Monad                  ( Monad((>>=))
                                                , (=<<)
                                                )
import           Data.Function                  ( flip
                                                , const
                                                , ($)
                                                )
import           Control.Applicative            ( Applicative(pure) )
import           Data.Functor                   ( void )
import           Data.Bool                      ((||),  Bool(..)
                                                , not
                                                )
import           Data.Witherable                ( Filterable(mapMaybe, filter) )
import           Data.Functor                   ( (<$>) )
import qualified Data.Text as Text
import Text.Show (Show(show))
import Control.Lens (view)
import qualified AdminPages
import Data.Ord (Ord((>=)))
import Reflex.Dom (Reflex(Event))
import Data.Functor (Functor(fmap))
import Data.Witherable (Filterable(catMaybes))
import GHC.Err (error)
import Obelisk.Route (renderFrontendRoute)
import Obelisk.Route (checkEncoder)
import Data.Eq (Eq)
import Data.Functor.Misc (Const2(Const2))

data FanAdmin
  = FanAdminLogin
  | FanAdminForbidden
  | FanAdminAccess
  deriving (Eq, Ord)

frontend :: Frontend (R FrontendRoute)
frontend =
    Frontend { _frontend_head = frontendHead, _frontend_body = frontendBody }

frontendBody
    :: forall t (m :: * -> *)
     . (ObeliskWidget t (R FrontendRoute) m)
    => RoutedT t (R FrontendRoute) m ()
frontendBody = mdo
    let lsAppState = "appState" :: Text
        lsSession = "session" :: Text
        lsRetrieve key = currentWindowUnchecked >>= getLocalStorage >>= \s -> getItem s key
        lsPut key d = currentWindowUnchecked >>= getLocalStorage >>= \s -> setItem s key d

        lsDecode :: forall a. FromJSON a => Text -> Maybe a
        lsDecode = Aeson.decode <<< Lazy.encodeUtf8 <<< Lazy.fromStrict

        lsEncode :: forall a. ToJSON a => a -> Text
        lsEncode = Lazy.toStrict <<< Lazy.decodeUtf8 <<< Aeson.encode

    evPb <- getPostBuild
    evDelayedPb <- delay 0.1 evPb
    evLatePb <- delay 1.0 evPb

    (evSessionInitial :: Event t (Maybe Session)) <-
      fmap switchDyn $ prerender (pure never) $
        -- TODO: why do I need to rely on postbuild?
        -- TODO: why do I need to rely on delayed postbuild?
        -- if I don't (i.e. relying on the switchover event), evSessionInitial never fires
        performEvent $ evDelayedPb $> fmap (lsDecode =<<) (liftJSM $ lsRetrieve lsSession)

    let evMaybeLocal = evSessionInitial <&> \case
          Just (SessionUser _) -> Nothing
          Just SessionAnon     -> Just True
          Nothing              -> Just False
    let evMaybeFromServer = evSessionInitial <&> \case
          Just (SessionUser sd) -> Just sd
          Just SessionAnon -> Nothing
          Nothing -> Nothing
        evSessionFromServer = void $ filter isJust evMaybeFromServer
    dynSessionData <- holdDyn Nothing $ Just <$> catMaybes evMaybeFromServer
    let dynAuthData = dynSessionData <&> \case
          Nothing -> Left "not logged in"
          Just SessionData{..} -> Right (sdJwt, sdAliasName)

    evRespServerSession <-
      request $ getAppState dynAuthData evSessionFromServer

    -- cannot get the app state: something is fishy with this session: reset
    let evAppStateInvalid = mapMaybe (either Just (const Nothing)) evRespServerSession
        evAppState = mapMaybe (either (const Nothing) Just) evRespServerSession
    prerender_ blank $ performEvent_ $ evAppStateInvalid $> liftJSM (lsPut lsSession $ lsEncode SessionAnon)

    let evSessionLocal = leftmost
          [ catMaybes evMaybeLocal
          , evAppStateInvalid $> False
          ]
    let (evLoadedFromServer :: Event t (Endo State)) =
           attach (current dynSessionData) evAppState <&> \case
             (Just sd, stApp) ->
               let stRedirectUrl = FrontendRoute_Main :/ ()
                   stSession = SessionUser sd
               in  Endo $ const State{..}
             (Nothing, _) -> error "impossible"

    (evLoadedLocal :: Event t (Endo State)) <-
      fmap (Endo . const) . switchDyn <$> prerender (pure never) do
        let stRedirectUrl = FrontendRoute_Main :/ ()
            stSession = SessionAnon
        performEvent $ evSessionLocal <&> \hasSession -> do
          stApp <- fromMaybe defaultAppState <$> if hasSession
                then fmap (lsDecode =<<) $ liftJSM $ lsRetrieve lsAppState
                else pure Nothing
          pure State {..}

    let evLoaded = leftmost [evLoadedFromServer, evLoadedLocal]
    dynHasLoaded <- holdDyn False $ evLoaded $> True
    dyn_ $ dynHasLoaded <&> \hasLoaded ->
      if hasLoaded then blank else loadingScreen ""

    dynState <- foldDyn appEndo defaultState $ leftmost [evLoaded, eStateUpdate]

    -- TODO: persist application state on visibility change (when hidden)
    --eUpdated <- tailE $ updated dynState

    let isLoggedIn = \State{..} -> case stSession of
          SessionUser _ -> True
          SessionAnon   -> False
        dynEAuth = dynState <&> \State{..} -> case stSession of
          SessionAnon -> Left "not logged in"
          SessionUser SessionData{..} -> Right (sdJwt, sdAliasName)

    updatedTail <- tailE $ updated dynState
    _ <- request $ postAppState dynEAuth (Right . stApp <$> dynState) $
      void $ filter isLoggedIn updatedTail

    prerender_ blank $ performEvent_ $
      filter (not <<< isLoggedIn) (updated dynState) <&> \State{..} ->
        liftJSM $ lsPut lsAppState $ lsEncode stApp

    prerender_ blank $ performEvent_ $ updated dynState <&> \State{..} -> do
        liftJSM $ lsPut lsSession  $ lsEncode stSession

    (_, eStateUpdate) <- mapRoutedT (runEventWriterT <<< flip runReaderT dynState) do

        message
        subRoute_ $ \case
            FrontendRoute_Main -> do
              requestPostViewPage $ constDyn $ FrontendRoute_Main :/ ()
              landingPage
            FrontendRoute_EN -> do
              dynRoute <- askRoute
              requestPostViewPage $ stageUrl EN <$> dynRoute
              stages @EN.Key EN
            FrontendRoute_DE -> do
              dynRoute <- askRoute
              requestPostViewPage $ stageUrl EN <$> dynRoute
              stages @DE.Key DE
            FrontendRoute_Auth -> subRoute_ \case
                AuthPage_SignUp -> do
                  requestPostViewPage $ constDyn $ FrontendRoute_Auth :/ AuthPage_SignUp :/ ()
                  AuthPages.signup
                AuthPage_Login  -> do
                  requestPostViewPage $ constDyn $ FrontendRoute_Auth :/ AuthPage_Login :/ ()
                  AuthPages.login
            FrontendRoute_Admin -> do
                evPbAdmin <- getPostBuild
                let evAdmin = attachWith const (current dynState) evPbAdmin
                    selector = fanMap $ evAdmin <&> \State{..} -> case stSession of
                      SessionAnon -> Map.singleton FanAdminLogin $ setRoute $ evPb $> FrontendRoute_Auth :/ AuthPage_Login :/ ()
                      SessionUser SessionData{..} ->
                        if sdIsSiteAdmin || sdClearances >= RankAdmin
                        then Map.singleton FanAdminAccess $ do
                               requestPostViewPage $ constDyn $ FrontendRoute_Admin :/ ()
                               AdminPages.journal
                        else Map.singleton FanAdminForbidden $ redirectToWikipedia "HTTP_403"
                    evLogin = select selector $ Const2 FanAdminLogin
                    evAccess = select selector $ Const2 FanAdminAccess
                    evForbidden = select selector $ Const2 FanAdminForbidden
                widgetHold_ (loadingScreen "") $ leftmost [evLogin, evAccess, evForbidden]
    blank

frontendHead
    :: forall t (m :: * -> *)
     . (ObeliskWidget t (R FrontendRoute) m)
    => RoutedT t (R FrontendRoute) m ()
frontendHead = do
    el "title" $ text "Palantype"
    elAttr
        "link"
        (  "href"
        =: $(static "main.css")
        <> "type"
        =: "text/css"
        <> "rel"
        =: "stylesheet"
        )
        blank
    elAttr
        "link"
        ("rel" =: "preconnect" <> "href" =: "https://fonts.googleapis.com")
        blank
    elAttr
        "link"
        (  "rel"
        =: "preconnect"
        <> "href"
        =: "https://fonts.gstatic.com"
        <> "crossorigin"
        =: "crossorigin"
        )
        blank
    elAttr
        "link"
        (  "href"
        =: "https://fonts.googleapis.com/css2?family=Abel&display=swap"
        <> "rel"
        =: "stylesheet"
        )
        blank
    elAttr
        "link"
        (  "href"
        =: "https://fonts.googleapis.com/css2?family=Special+Elite&display=swap"
        <> "rel"
        =: "stylesheet"
        )
        blank
    elAttr
        "link"
        (  "rel"
        =: "stylesheet"
        <> "href"
        =: $(static "FontAwesome/css/all.min.css")
        )
        blank
    elAttr
        "link"
        (  "rel"
        =: "stylesheet"
        <> "href"
        =: $(static "flag-icons-main/css/flag-icons.min.css")
        )
        blank
