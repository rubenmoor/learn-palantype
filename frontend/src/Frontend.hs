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
import           Control.Lens.Setter            ( (?~) )
import           Data.Generics.Product          ( field )
import           Language.Javascript.JSaddle    ( liftJSM )
import           State                          ( updateState
                                                , Session(..)
                                                , State(..)
                                                , defaultState
                                                , stageUrl
                                                )

import           Common.Route                   ( FrontendRoute(..)
                                                , FrontendRoute_AuthPages(..)
                                                )
import           Control.Monad.Reader           ( ReaderT(runReaderT) )
import qualified Data.Aeson                    as Aeson
import           Data.Functor                   ( ($>)
                                                , (<&>)
                                                )
import           Data.Function                  ( (.) )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( Maybe(..)
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
import           GHCJS.DOM.Window               ( getLocalStorage )
import           Home                           ( message
                                                , settings
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
import           Obelisk.Route.Frontend         ( RoutedT
                                                , SetRoute(setRoute)
                                                , mapRoutedT
                                                , subRoute_
                                                )
import           Palantype.Common               ( Lang(..) )
import qualified Palantype.DE.Keys             as DE
import qualified Palantype.EN.Keys             as EN
import           Reflex.Dom                     (constDyn,  zipDyn
                                                , holdDyn
                                                , tag
                                                , prerender
                                                , current
                                                , attach
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

import           Shared                         ( loadingScreen )
import qualified AuthPages
import           Control.Category               ( (<<<) )
import           Common.Model                   ( Message(..)
                                                , defaultAppState
                                                )
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           Common.Auth                    ( SessionData(..) )
import           Client                         (getAuthData, getMaybeAuthData, postEventViewPage,  postAppState
                                                , request
                                                , getAppState
                                                )
import           Data.Either                    ( fromRight
                                                , either
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
import           Data.Bool                      ( Bool(..)
                                                , not
                                                )
import           Data.Witherable                ( Filterable(mapMaybe, filter) )
import           Control.Monad                  ( join )
import           Data.Functor                   ( (<$>) )
import qualified Data.Text as Text
import Text.Show (Show(show))

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

    dynMSession <- prerender (pure Nothing) do
        mStrSession <- liftJSM $ lsRetrieve lsSession
        pure $ Just $ lsDecode =<< mStrSession

    let evMSession = updated dynMSession

        dynEAuthInitial = dynMSession <&> \case
          Just (Just (SessionUser SessionData{..})) -> Right (sdJwt, sdAliasName)
          Just _                                    -> Left "not logged in"
          Nothing                                   -> Left "not yet loaded"

        isLoadedAndLoggedIn = \case
          Just (Just (SessionUser _)) -> True
          _                           -> False

    evRespAppState <- request $ getAppState dynEAuthInitial $ void $ filter isLoadedAndLoggedIn evMSession
    dynLSAppState <- prerender (pure defaultAppState) do
        mStrState <- liftJSM $ lsRetrieve lsAppState
        pure $ fromMaybe defaultAppState $ lsDecode =<< mStrState

    let evLSAppState = tag (current dynLSAppState) $ filter (not <<< isLoadedAndLoggedIn) evMSession
        evLoaded = attach (current dynMSession) (leftmost
          [ fromRight defaultAppState <$> evRespAppState
          , evLSAppState
          ]) <&> \(mSession, stApp) ->
              let stSession = fromMaybe SessionAnon $ join mSession
                  stRedirectUrl = FrontendRoute_Main :/ ()
              in  Endo $ const State{..}

        evSessionInvalid = mapMaybe (either Just (const Nothing)) evRespAppState

    -- in case there is a problem: delete session
    prerender_ blank $ performEvent_ $
      evSessionInvalid $> liftJSM (lsPut lsSession $ lsEncode SessionAnon)

    dynHasLoaded <- holdDyn False $ evLoaded $> True
    dyn_ $ zipDyn dynHasLoaded dynMSession <&> \case
            (True, _) -> blank
            (False, mSession) -> loadingScreen $ case mSession of
                Nothing                                   -> ""
                Just Nothing                              -> "You seem to be new here. Welome!"
                Just (Just SessionAnon)                   -> "Welcome back!"
                Just (Just (SessionUser SessionData{..})) -> "Hi, " <> sdAliasName <> "!"

    dynState <- foldDyn appEndo defaultState $ leftmost [evLoaded, eStateUpdate]

    -- TODO: persist application state on visibility change (when hidden)
    eUpdated <- tailE $ updated dynState

    let isLoggedIn = \State{..} -> case stSession of
          SessionUser _ -> True
          SessionAnon   -> False
        dynEAuth = dynState <&> \State{..} -> case stSession of
          SessionAnon -> Left "not logged in"
          SessionUser SessionData{..} -> Right (sdJwt, sdAliasName)

    _ <- request $ postAppState dynEAuth (Right . stApp <$> dynState) $
      void $ filter isLoggedIn eUpdated

    prerender_ blank $ performEvent_ $
      filter (not <<< isLoggedIn) eUpdated <&> \State{..} ->
        liftJSM $ lsPut lsAppState $ lsEncode stApp

    prerender_ blank $ performEvent_ $ eUpdated <&> \State{..} -> do
        liftJSM $ lsPut lsSession  $ lsEncode stSession

    (_, eStateUpdate) <- mapRoutedT (runEventWriterT <<< flip runReaderT dynState) do
        message
        ePb <- getPostBuild
        subRoute_ $ \case
            FrontendRoute_Main -> dyn_ $ dynState <&> \st -> do
               -- go to url where the user left the last time
               let mUrl = do
                       lang  <- st ^. field @"stApp" . field @"stMLang"
                       stage <- Map.lookup lang $  st ^. field @"stApp" .  field @"stProgress"
                       pure $ stageUrl lang stage
               case mUrl of
                   Just url -> do setRoute $ ePb $> url

                   -- or show the landing page
                   Nothing -> do
                     request $ postEventViewPage (constDyn $ getMaybeAuthData st)
                                                 (constDyn $ Right $ Text.pack $ show $ FrontendRoute_Main :/ ())
                                                 ePb
                     landingPage
            FrontendRoute_EN -> do
                el "header" $ settings EN
                stages @EN.Key EN
            FrontendRoute_DE -> do
                el "header" $ settings DE
                stages @DE.Key DE
            FrontendRoute_Auth -> subRoute_ \case
                AuthPage_SignUp -> do
                  request $ postEventViewPage (getMaybeAuthData <$> dynState)
                                              (constDyn $ Right $ Text.pack $ show $ FrontendRoute_Auth :/ AuthPage_SignUp :/ ())
                                              ePb
                  AuthPages.signup
                AuthPage_Login  -> AuthPages.login
        updateState $ evSessionInvalid <&> \str ->
          [ field @"stApp" . field @"stMsg" ?~
              Message "Session invalid" ("Please log in again. " <> str)
          ]
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
