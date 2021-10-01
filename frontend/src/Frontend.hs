{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Frontend where

import           Language.Javascript.JSaddle (liftJSM)
import           State                       (EStateUpdate (..), State (..),
                                              stageUrl)

import           Common.Route                (FrontendRoute (..))
import           Control.Monad.Reader        (ReaderT (runReaderT), withReaderT)
import qualified Data.Aeson                  as Aeson
import           Data.Functor                (($>), (<&>))
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)
import qualified Data.Text.Lazy              as Lazy
import qualified Data.Text.Lazy.Encoding     as Lazy
import           GHCJS.DOM                   (currentWindowUnchecked)
import           GHCJS.DOM.Storage           (getItem, setItem)
import           GHCJS.DOM.Window            (getLocalStorage)
import           Home                        (loadingScreen, message, settings,
                                              stenoInput)
import           Obelisk.Frontend            (Frontend (..), ObeliskWidget)
import           Obelisk.Generated.Static    (static)
import           Obelisk.Route               (R)
import           Obelisk.Route.Frontend      (RoutedT,
                                              SetRoute (setRoute), mapRoutedT,
                                              subRoute_)
import           Reflex.Dom                  (dyn_, PostBuild(getPostBuild), PerformEvent (performEvent_),
                                              Prerender (prerender),
                                              Reflex (updated), blank,
                                              def, el, elAttr, foldDyn,
                                              leftmost, prerender_,
                                              runEventWriterT, tailE, text,
                                              widgetHold_, (=:))
import           Stage                       (stage1_1, stage1_2, stage1_3,
                                              stage1_4, stage1_5, stage2_1)

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = frontendHead
  , _frontend_body = frontendBody
  }

-- instance (Monad m, SetRoute t r m) => SetRoute t r (RandT g m)
-- instance DomBuilder t m => DomBuilder t (RandT g m) where
--     type DomBuilderSpace (RandT g m) = DomBuilderSpace m

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
  prerender_ blank $ performEvent_ $ eUpdated <&> \st -> do
    let str = Lazy.toStrict $ Lazy.decodeUtf8 $ Aeson.encode st
    liftJSM (currentWindowUnchecked >>= getLocalStorage >>= setState str)

  (_, eStateUpdate) <- mapRoutedT (runEventWriterT . flip runReaderT dynState ) $ do
    settings
    message
    eWord <- stenoInput
    mapRoutedT (withReaderT (,eWord)) $
      subRoute_ $ \case
        FrontendRoute_Main ->
          dyn_ $ fmap stProgress dynState <&> \stage -> do
            ePb <- getPostBuild
            setRoute $ ePb $> stageUrl stage
        FrontendRoute_Stage1_1 -> stage1_1
        FrontendRoute_Stage1_2 -> stage1_2
        FrontendRoute_Stage1_3 -> stage1_3
        FrontendRoute_Stage1_4 -> stage1_4
        FrontendRoute_Stage1_5 -> stage1_5
        FrontendRoute_Stage2_1 -> stage2_1
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
