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

module Frontend where

import Language.Javascript.JSaddle (liftJSM)
import State (
    State (..),
    stageUrl,
 )

import Common.Route (FrontendRoute (..))
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.Aeson as Aeson
import Data.Data (Proxy (Proxy))
import Data.Functor (
    ($>),
    (<&>),
 )
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup (Endo (..))
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as Lazy
import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.Storage (
    getItem,
    setItem,
 )
import GHCJS.DOM.Window (getLocalStorage)
import Home (
    landingPage,
    stages,
 )
import Obelisk.Frontend (
    Frontend (..),
    ObeliskWidget,
 )
import Obelisk.Generated.Static (static)
import Obelisk.Route (R)
import Obelisk.Route.Frontend (
    RoutedT,
    SetRoute (setRoute),
    mapRoutedT,
    subRoute_,
 )
import Palantype.Common (Lang (..))
import qualified Palantype.DE.Keys as DE
import qualified Palantype.EN.Keys as EN
import Reflex.Dom (
    PerformEvent (performEvent_),
    PostBuild (getPostBuild),
    Prerender (prerender),
    Reflex (updated),
    blank,
    def,
    dyn_,
    el,
    elAttr,
    foldDyn,
    leftmost,
    prerender_,
    runEventWriterT,
    tailE,
    text,
    widgetHold_,
    (=:),
 )
import Shared (loadingScreen)

frontend :: Frontend (R FrontendRoute)
frontend =
    Frontend{_frontend_head = frontendHead, _frontend_body = frontendBody}

frontendBody ::
    forall t js (m :: * -> *).
    (ObeliskWidget js t (R FrontendRoute) m) =>
    RoutedT t (R FrontendRoute) m ()
frontendBody = mdo
    let key = "state" :: Text
        getState s = getItem s key
        setState d s = setItem s key d

    dynLoadState <- prerender (pure $ Endo $ const (def :: State)) $ do
        mStr <-
            liftJSM
                (currentWindowUnchecked >>= getLocalStorage >>= getState)
        let mState = mStr >>= Aeson.decode . Lazy.encodeUtf8 . Lazy.fromStrict
        pure $ Endo $ const $ fromMaybe def mState

    let eLoaded = updated dynLoadState
    widgetHold_ loadingScreen $ eLoaded $> blank

    dynState <- foldDyn appEndo def $ leftmost [eLoaded, eStateUpdate]

    -- TODO: persist application state on visibility change (when hidden)
    eUpdated <- tailE $ updated dynState
    prerender_ blank $
        performEvent_ $
            eUpdated <&> \st -> do
                let str = Lazy.toStrict $ Lazy.decodeUtf8 $ Aeson.encode st
                liftJSM (currentWindowUnchecked >>= getLocalStorage >>= setState str)

    (_, eStateUpdate) <-
        mapRoutedT (runEventWriterT . flip runReaderT dynState) $ do
            subRoute_ $ \case
                FrontendRoute_Main -> do
                    dyn_ $
                        dynState <&> \State{..} -> do
                            -- go to url where the user left the last time
                            let mUrl = do
                                    lang <- stMLang
                                    stage <- Map.lookup lang stProgress
                                    pure $ stageUrl lang stage
                            case mUrl of
                                Just url -> do
                                    ePb <- getPostBuild
                                    setRoute $ ePb $> url

                                -- or show the landing page
                                Nothing -> landingPage
                FrontendRoute_EN -> stages (Proxy :: Proxy EN.Key) EN
                FrontendRoute_DE -> stages (Proxy :: Proxy DE.Key) DE
    blank

frontendHead ::
    forall t js (m :: * -> *).
    (ObeliskWidget js t (R FrontendRoute) m) =>
    RoutedT t (R FrontendRoute) m ()
frontendHead = do
    el "title" $ text "Palantype"
    elAttr
        "link"
        ( "href"
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
        ( "rel"
            =: "preconnect"
            <> "href"
            =: "https://fonts.gstatic.com"
            <> "crossorigin"
            =: "crossorigin"
        )
        blank
    elAttr
        "link"
        ( "href"
            =: "https://fonts.googleapis.com/css2?family=Abel&display=swap"
            <> "rel"
            =: "stylesheet"
        )
        blank
    elAttr
        "link"
        ( "rel"
            =: "stylesheet"
            <> "href"
            =: $(static "FontAwesome/css/all.min.css")
        )
        blank
    elAttr
        "link"
        ( "rel"
            =: "stylesheet"
            <> "href"
            =: $(static "flag-icons-main/css/flag-icons.min.css")
        )
        blank
