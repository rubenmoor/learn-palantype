{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Frontend where

import           Language.Javascript.JSaddle (liftJSM)
import           State                       (Stage(..),  EStateUpdate (..), State (..),
                                              stageUrl, Env(..), Navigation (..))

import           Common.Route                (FrontendRoute (..))
import           Control.Monad.Reader        (ReaderT (runReaderT), withReaderT)
import qualified Data.Aeson                  as Aeson
import           Data.Functor                (void, ($>), (<&>))
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)
import qualified Data.Text.Lazy              as Lazy
import qualified Data.Text.Lazy.Encoding     as Lazy
import           GHCJS.DOM                   (currentWindowUnchecked)
import           GHCJS.DOM.Storage           (getItem, setItem)
import           GHCJS.DOM.Window            (getLocalStorage)
import           Home                        (toc, message, settings,
                                              stenoInput)
import           Obelisk.Frontend            (Frontend (..), ObeliskWidget)
import           Obelisk.Generated.Static    (static)
import           Obelisk.Route               (R)
import           Obelisk.Route.Frontend      (subRoute, RoutedT,
                                              SetRoute (setRoute), mapRoutedT,
                                              subRoute_)
import           Reflex.Dom                  (elClass, EventWriterT, dyn_, PostBuild(getPostBuild), PerformEvent (performEvent_),
                                              Prerender (prerender),
                                              Reflex(Dynamic, Event, updated), blank,
                                              def, el, elAttr, foldDyn,
                                              leftmost, prerender_,
                                              runEventWriterT, tailE, text,
                                              widgetHold_, (=:))
import           Stage                       (stage1_7, stage1_6, introduction, stage1_1, stage1_2, stage1_3,
                                              stage1_4, stage1_5, stage2_1, elFooter)
import Shared (loadingScreen)

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
  prerender_ blank $ performEvent_ $ eUpdated <&> \st -> do
    let str = Lazy.toStrict $ Lazy.decodeUtf8 $ Aeson.encode st
    liftJSM (currentWindowUnchecked >>= getLocalStorage >>= setState str)

  (_, eStateUpdate) <- mapRoutedT (runEventWriterT . flip runReaderT dynState ) $ do

    elClass "div" "box" $ do
      eChord <- el "header" $ do
        settings
        message
        stenoInput

      dynNavigation <-
        elClass "div" "row" $ mdo

          toc $ navCurrent <$> dynNavigation

          let setEnv
                :: forall a.
                   Maybe Stage
                -> Stage
                -> Maybe Stage
                -> RoutedT t a (ReaderT (Env t) (EventWriterT t EStateUpdate m)) Navigation
                -> RoutedT t a (ReaderT (Dynamic t State) (EventWriterT t EStateUpdate m)) Navigation
              setEnv navMPrevious navCurrent navMNext =
                mapRoutedT (withReaderT $ \_ -> Env
                  { envDynState = dynState
                  , envEChord = eChord
                  , envNavigation = Navigation{..}
                  })
          dynNavigation <-
            elClass "section" "content" $ subRoute $ \case

              FrontendRoute_Main -> do
                dyn_ $ dynState <&> \st -> do
                  ePb <- getPostBuild
                  setRoute $ ePb $> stageUrl (stProgress st)

                -- meaningless
                pure $ Navigation Nothing Stage1_1 Nothing

              FrontendRoute_Introduction -> setEnv Nothing Introduction (Just Stage1_1) introduction
              FrontendRoute_Stage1_1 -> setEnv (Just Introduction) Stage1_1 (Just Stage1_2) stage1_1
              FrontendRoute_Stage1_2 -> setEnv (Just Stage1_1) Stage1_2 (Just Stage1_3) stage1_2
              FrontendRoute_Stage1_3 -> setEnv (Just Stage1_2) Stage1_3 (Just Stage1_4) stage1_3
              FrontendRoute_Stage1_4 -> setEnv (Just Stage1_3) Stage1_4 (Just Stage1_5) stage1_4
              FrontendRoute_Stage1_5 -> setEnv (Just Stage1_4) Stage1_5 (Just Stage1_6) stage1_5
              FrontendRoute_Stage1_6 -> setEnv (Just Stage1_5) Stage1_6 (Just Stage1_7) stage1_6
              FrontendRoute_Stage1_7 -> setEnv (Just Stage1_6) Stage1_7 (Just Stage2_1) stage1_7
              FrontendRoute_Stage2_1 -> setEnv (Just Stage1_7) Stage2_1 Nothing         stage2_1
          pure dynNavigation
      dyn_ $ dynNavigation <&> elFooter
  blank

frontendHead
  :: forall t js (m :: * -> *).
  (ObeliskWidget js t (R FrontendRoute) m
  )
  => RoutedT t (R FrontendRoute) m ()
frontendHead = do
  el "title" $ text "Palantype"
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
