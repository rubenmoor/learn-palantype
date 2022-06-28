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
import qualified Data.Aeson                    as Aeson
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
import           Obelisk.Route.Frontend         (RoutedT
                                                , SetRoute(setRoute)
                                                , mapRoutedT
                                                , subRoute_
                                                )
import           Palantype.Common               ( Lang(..) )
import qualified Palantype.DE.Keys             as DE
import qualified Palantype.EN.Keys             as EN
import           Reflex.Dom                     (attachPromptlyDynWith, select, fanMap, gate, current, attach, constDyn
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
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           Common.Auth                    ( SessionData(..) )
import           Client                         (postAppState
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
import qualified AdminPages
import Data.Ord (Ord((>=)))
import Reflex.Dom (Reflex(Event))
import Data.Functor (Functor(fmap))
import Data.Witherable (Filterable(catMaybes))
import GHC.Err (error)
import Data.Eq (Eq)
import Data.Functor.Misc (Const2(Const2))
import Palantype.Common.TH (failure)

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

    -- evPb <- getPostBuild
    -- (evSessionInitial :: Event t (Maybe (Session, AppState))) <-
    --   switchDyn <$> prerender (pure never)
    --     ( performEvent $ evPb $> do
    --         mSession <- fmap (lsDecode =<<) (liftJSM $ lsRetrieve lsSession)
    --         stApp    <- fromMaybe defaultAppState . (lsDecode =<<) <$> liftJSM (lsRetrieve lsAppState)
    --         pure $ (,stApp) <$> mSession
    --     )
    (evSessionInitial :: Event t (Maybe (Session, AppState))) <-
      updated <$> prerender (pure $ $failure "unexpected") do
            mSession <- fmap (lsDecode =<<) (liftJSM $ lsRetrieve lsSession)
            stApp    <- fromMaybe defaultAppState . (lsDecode =<<) <$> liftJSM (lsRetrieve lsAppState)
            pure $ (,stApp) <$> mSession

    let evMaybeLocal = evSessionInitial <&> \case
          Just (SessionUser _, _       ) -> Nothing
          Just (SessionAnon  , appState) -> Just (Just appState)
          Nothing                        -> Just Nothing

        evMaybeFromServer = evSessionInitial <&> \case
          Just (SessionUser sd, _) -> Just sd
          Just (SessionAnon   , _) -> Nothing
          Nothing                  -> Nothing

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
          , evAppStateInvalid $> Nothing
          ]

        (evLoadedFromServer :: Event t (Endo State)) =
           attach (current dynSessionData) evAppState <&> \case
             (Just sd, stApp) ->
               let stRedirectUrl = FrontendRoute_Main :/ ()
                   stSession = SessionUser sd
               in  Endo $ const State{..}
             (Nothing, _) -> error "impossible"

        evLoadedLocal = fmap (Endo . const) $ evSessionLocal <&> \mAppState ->
            let stRedirectUrl = FrontendRoute_Main :/ ()
                stSession = SessionAnon
                stApp = fromMaybe defaultAppState mAppState
            in  State{..}

    let evLoaded = leftmost [evLoadedFromServer, evLoadedLocal]
    dynHasLoaded <- holdDyn False $ evLoaded $> True

    let toReady evPb = leftmost [gate (current dynHasLoaded) evPb, void evLoaded]

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
                    evLogin = select selector $ Const2 FanAdminLogin
                    -- evAccess = select selector $ Const2 FanAdminAccess
                    evForbidden = select selector $ Const2 FanAdminForbidden
                setRoute $ evLogin $> FrontendRoute_Auth :/ AuthPage_Login :/ ()
                prerender_ blank $ performEvent_ $ evForbidden $> redirectToWikipedia "HTTP_403"

                requestPostViewPage (constDyn $ FrontendRoute_Admin :/ ()) $ void evReady
                AdminPages.journal
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
