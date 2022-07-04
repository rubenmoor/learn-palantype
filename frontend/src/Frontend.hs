{-# LANGUAGE TupleSections #-}
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

import           Language.Javascript.JSaddle    ( liftJSM )
import           State                          (Session(..)
                                                , State(..)
                                                , defaultState

                                                )

import           Common.Route                   (FrontendRoute(..)
                                                , FrontendRoute_AuthPages(..)
                                                )
import           Control.Monad.Reader           (ReaderT(runReaderT) )
import           Data.Functor                   ( ($>)
                                                , (<&>)
                                                )
import           Data.Function                  ( (.) )
import qualified Data.Map                      as Map
import           Data.Maybe                     (isJust,  Maybe(..)
                                                , fromMaybe
                                                )
import           Data.Semigroup                 ( (<>)
                                                , Endo(..)
                                                )
import           Data.Text                      ( Text )
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
import           Obelisk.Route.Frontend         (RoutedT
                                                , SetRoute(setRoute)
                                                , mapRoutedT
                                                , subRoute_
                                                )
import           Palantype.Common               ( Lang(..) )
import qualified Palantype.DE.Keys             as DE
import qualified Palantype.EN.Keys             as EN
import           Reflex.Dom                     (widgetHold_, zipDyn, holdUniqDyn, fanEither, attachPromptlyDynWith, select, fanMap, gate, current, attach, constDyn
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
import           Common.Model                   (AppState, Rank (RankAdmin)
                                                , defaultAppState
                                                )
import           Common.Auth                    ( SessionData(..) )
import           Client                         (postAppState , request , getAppState
                                                )
import           Data.Either                    ( Either(..)
                                                )
import           Control.Monad                  ( Monad((>>=))

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
import           Data.Witherable                ( Filterable(filter) )
import           Data.Functor                   ( (<$>) )
import qualified AdminPages
import Data.Ord (Ord((>=)))
import Reflex.Dom (Reflex(Event))
import Data.Functor (Functor(fmap))
import Data.Witherable (Filterable(catMaybes))
import Data.Eq ((==), Eq)
import Data.Functor.Misc (Const2(Const2))
import Palantype.Common.TH (failure)
import qualified LocalStorage as LS

default(Text)

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
    (evSessionInitial :: Event t (Maybe (Session, AppState))) <-
      updated <$> prerender (pure $ $failure "unexpected") do
            mSession <- liftJSM $ LS.retrieve LS.KeySession
            appState <- fromMaybe defaultAppState <$> liftJSM (LS.retrieve LS.KeyAppState)
            pure $ (,appState) <$> mSession

    let evMaybeLocal = evSessionInitial <&> \case
          Just (SessionUser _, _       ) -> Nothing
          Just (SessionAnon  , appState) -> Just (Just appState)
          Nothing                        -> Just Nothing

        evMaybeFromServer = evSessionInitial <&> \case
          Just (SessionUser sd, _) -> Just sd
          Just (SessionAnon   , _) -> Nothing
          Nothing                     -> Nothing

        evSessionFromServer = void $ filter isJust evMaybeFromServer

    dynSessionData <- holdDyn Nothing $ Just <$> catMaybes evMaybeFromServer

    let dynAuthData = dynSessionData <&> \case
          Nothing -> Left "not logged in"
          Just SessionData{..} -> Right (sdJwt, sdAliasName)

    evRespServerSession <-
      request $ getAppState dynAuthData evSessionFromServer

    let (evAppStateInvalid, evAppState) = fanEither evRespServerSession

    -- if we cannot get the app state, something is fishy with this session: reset
    prerender_ blank $ performEvent_ $ evAppStateInvalid $>
      liftJSM (LS.put LS.KeySession SessionAnon)

    let evSessionLocal = leftmost
          [ catMaybes evMaybeLocal
          , evAppStateInvalid $> Nothing
          ]

        (evLoadedFromServer :: Event t (Endo State)) =
           attach (current dynSessionData) evAppState <&> \case
             (Just sd, stApp) ->
               let stRedirectUrl = FrontendRoute_Main :/ ()
                   stSession = SessionUser sd
               in  Endo $ const State{..}
             (Nothing, _) -> $failure "impossible"

        evLoadedLocal = fmap (Endo . const) $ evSessionLocal <&> \mAppState ->
            let stRedirectUrl = FrontendRoute_Main :/ ()
                stSession = SessionAnon
                stApp = fromMaybe defaultAppState mAppState
            in  State{..}

    let evLoaded = leftmost
            [ evLoadedFromServer
            , evLoadedLocal
            ]
    dynHasLoaded <- holdDyn False $ evLoaded $> True

    let toReady evPb = leftmost [gate (current dynHasLoaded) evPb, void evLoaded]

    dyn_ $ dynHasLoaded <&> \hasLoaded ->
      if hasLoaded then blank else loadingScreen ""

    dynState <- foldDyn appEndo defaultState $ leftmost [evLoaded, eStateUpdate]

    let isLoggedIn = \State{..} -> case stSession of
          SessionUser _ -> True
          SessionAnon   -> False
        dynEAuth = dynState <&> \State{..} -> case stSession of
          SessionAnon -> Left "not logged in"
          SessionUser SessionData{..} -> Right (sdJwt, sdAliasName)

    -- TODO: persist application state on visibility change (when hidden)
    updatedTail <- tailE $ updated dynState
    _ <- request $ postAppState dynEAuth (Right . stApp <$> dynState) $
        void $ filter isLoggedIn updatedTail

    prerender_ blank $ performEvent_ $
      filter (not <<< isLoggedIn) (updated dynState) <&> \State{..} ->
        liftJSM $ LS.put LS.KeyAppState stApp

    prerender_ blank $ performEvent_ $ updated dynState <&> \State{..} -> do
        liftJSM $ LS.put LS.KeySession stSession

    (_, eStateUpdate) <- mapRoutedT (runEventWriterT <<< flip runReaderT dynState) do

        message
        subRoute_ $ \r -> do
          case r of
            FrontendRoute_Main -> do
              toReady <$> getPostBuild >>=
                requestPostViewPage (constDyn $ FrontendRoute_Main :/ ())
              landingPage
            FrontendRoute_EN -> stages @EN.Key EN toReady
            FrontendRoute_DE -> stages @DE.Key DE toReady
            FrontendRoute_Auth -> subRoute_ \case
                AuthPage_SignUp -> do
                  toReady <$> getPostBuild >>=
                    requestPostViewPage (constDyn $ FrontendRoute_Auth :/ AuthPage_SignUp :/ ())
                  AuthPages.signup
                AuthPage_Login  -> do
                  toReady <$> getPostBuild >>=
                    requestPostViewPage (constDyn $ FrontendRoute_Auth :/ AuthPage_Login :/ ())
                  AuthPages.login
                AuthPage_Settings -> do
                  dynSettings <- holdUniqDyn $ zipDyn dynHasLoaded (isLoggedIn <$> dynState) <&> \case
                    (False, _        ) -> Nothing
                    (True , bLoggedIn) -> Just bLoggedIn
                  let evToLogin = filter (== Just False) $ updated dynSettings
                      evToSettings = filter (== Just True) $ updated dynSettings
                  setRoute $ evToLogin $> FrontendRoute_Auth :/ AuthPage_Login :/ ()

                  requestPostViewPage (constDyn $ FrontendRoute_Auth :/ AuthPage_Settings :/ ()) $ void evToSettings
                  dyn_ $ dynSettings <&> \case
                    Just True -> AuthPages.settings
                    _         -> blank
            FrontendRoute_Admin -> do
                evReady <- toReady <$> getPostBuild
                let
                    evAdmin = attachPromptlyDynWith const dynState evReady
                    selector = fanMap $ evAdmin <&> \State{..} -> case stSession of
                      SessionAnon -> Map.singleton FanAdminLogin ()
                      SessionUser SessionData{..} ->
                        if sdIsSiteAdmin || sdClearances >= RankAdmin
                        then Map.singleton FanAdminAccess ()
                        else Map.singleton FanAdminForbidden ()
                    evToLogin = select selector $ Const2 FanAdminLogin
                    evAccess = select selector $ Const2 FanAdminAccess
                    evForbidden = select selector $ Const2 FanAdminForbidden

                setRoute $ evToLogin $> FrontendRoute_Auth :/ AuthPage_Login :/ ()
                prerender_ blank $ performEvent_ $ evForbidden $> redirectToWikipedia "HTTP_403"

                requestPostViewPage (constDyn $ FrontendRoute_Admin :/ ()) evAccess
                AdminPages.journal dynHasLoaded
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
