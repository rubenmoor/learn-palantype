{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Frontend where

import           Data.Generics.Product       (field)
import           Language.Javascript.JSaddle (liftJSM)
import           State                       (EStateUpdate (..), State (..),
                                              updateState)

import           Obelisk.Frontend            (Frontend (..), ObeliskWidget)
import           Obelisk.Generated.Static    (static)
import           Obelisk.Route               (R)

import           Reflex.Dom.Core             (blank, el, elAttr, text, (=:))

import           Common.Route                (FrontendRoute (FrontendRoute_Main))
import           Control.Lens.Setter         ((.~))
import           Control.Monad.Reader        (MonadReader (ask),
                                              ReaderT (runReaderT), asks)
import qualified Data.Aeson                  as Aeson
import           Data.Functor                (($>))
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)
import qualified Data.Text.Lazy              as Lazy
import qualified Data.Text.Lazy.Encoding     as Lazy
import           GHCJS.DOM                   (currentWindowUnchecked)
import           GHCJS.DOM.Storage           (getItem, setItem)
import           GHCJS.DOM.Window            (getLocalStorage)
import           Home                        (keyboard, message, loadingScreen, settings)
import           Obelisk.Route.Frontend      (RoutedT, mapRoutedT, subRoute_)
import           Reflex.Dom                  (DomBuilder, EventName (Click),
                                              EventWriter,
                                              HasDomEvent (domEvent),
                                              PerformEvent (performEvent_),
                                              Prerender (prerender),
                                              Reflex (updated), def, dyn_,
                                              elAttr', ffor, foldDyn, leftmost,
                                              prerender_, runEventWriterT,
                                              tailE, widgetHold_)
import           Shared                      (iFa)

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = frontendHead
  , _frontend_body = frontendBody
  }

frontendBody
  :: forall t js (m :: * -> *).
  ( ObeliskWidget  js t (R FrontendRoute) m
  )
  => RoutedT t (R FrontendRoute) m ()
frontendBody = mdo

  let key = "state" :: Text
      getState s = getItem s key
      setState d s = setItem s key d

  dynLoadState <- prerender (pure $ EStateUpdate $ const def) $ do
    mStr <- liftJSM (currentWindowUnchecked >>= getLocalStorage >>= getState)
    let mState = mStr >>= Aeson.decode . Lazy.encodeUtf8. Lazy.fromStrict
    pure $ EStateUpdate $ const $ fromMaybe def mState

  let eLoaded = updated dynLoadState
  widgetHold_ loadingScreen $ eLoaded $> blank

  dynState <- foldDyn unEStateUpdate def $ leftmost [eLoaded, eStateUpdate]

  -- TODO: persist application state on visibility change (when hidden)
  eUpdated <- tailE $ updated dynState
  prerender_ blank $ performEvent_ $ ffor eUpdated $ \st -> do
    let str = Lazy.toStrict $ Lazy.decodeUtf8 $ Aeson.encode st
    liftJSM (currentWindowUnchecked >>= getLocalStorage >>= setState str)

  (_, eStateUpdate) <- mapRoutedT (flip runReaderT dynState . runEventWriterT) $ do
    settings
    message
    keyboard
    subRoute_ $ \case
      FrontendRoute_Main -> text "Hi (FrontendRoute_Main)"
  blank

frontendHead
  :: forall t js (m :: * -> *).
  (ObeliskWidget js t (R FrontendRoute) m
  )
  => RoutedT t (R FrontendRoute) m ()
frontendHead = do
  el "title" $ text "Palantype ftw"
  elAttr "link"
    (  "href" =: static @"main.css"
    <> "type" =: "text/css"
    <> "rel" =: "stylesheet"
    ) blank
  elAttr "link"
    (  "rel" =: "preconnect"
    <> "href" =: "https://fonts.googleapis.com"
    ) blank
  elAttr "link"
    (  "rel" =: "preconnect"
    <> "href" =: "https://fonts.gstatic.com"
    <> "crossorigin" =: "crossorigin"
    ) blank
  elAttr "link"
    (  "href" =: "https://fonts.googleapis.com/css2?family=Abel&display=swap"
    <> "rel" =: "stylesheet"
    ) blank
  elAttr "link"
    (  "rel" =: "stylesheet"
    <> "href" =: static @"FontAwesome/css/all.min.css"
    ) blank
