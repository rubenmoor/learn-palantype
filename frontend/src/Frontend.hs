{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Frontend where

import           Language.Javascript.JSaddle (liftJSM)
import           State                       (Env (..), Navigation (..),
                                              Stage (..), State (..), stageUrl)

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
import           Home                        (message, settings, stenoInput,
                                              toc)
import           Obelisk.Frontend            (Frontend (..), ObeliskWidget)
import           Obelisk.Generated.Static    (static)
import           Obelisk.Route               (R)
import           Obelisk.Route.Frontend      (RoutedT, SetRoute (setRoute),
                                              mapRoutedT, subRoute)
import           Page.Common                 (elFooter)
import           Page.Introduction           (introduction)
import qualified Page.Stage1                 as Stage1
import qualified Page.Stage2                 as Stage2
import           Reflex.Dom                  (EventWriterT,
                                              PerformEvent (performEvent_),
                                              PostBuild (getPostBuild),
                                              Prerender (prerender),
                                              Reflex (Dynamic, updated),
                                              blank, def, dyn_, el, elAttr,
                                              elClass, foldDyn, leftmost,
                                              prerender_, runEventWriterT,
                                              tailE, text, widgetHold_, (=:))
import           Shared                      (loadingScreen)
import Data.Semigroup (Endo (..))

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

  dynLoadState <- prerender (pure $ Endo $ const def) $ do
    mStr <- liftJSM (currentWindowUnchecked >>= getLocalStorage >>= getState)
    let mState = mStr >>= Aeson.decode . Lazy.encodeUtf8. Lazy.fromStrict
    pure $ Endo $ const $ fromMaybe def mState

  let eLoaded = updated dynLoadState
  widgetHold_ loadingScreen $ eLoaded $> blank

  dynState <- foldDyn appEndo def $ leftmost [eLoaded, eStateUpdate]

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
                -> RoutedT t a (ReaderT (Env t) (EventWriterT t (Endo State) m)) Navigation
                -> RoutedT t a (ReaderT (Dynamic t State) (EventWriterT t (Endo State) m)) Navigation
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
              FrontendRoute_Stage1_1 -> setEnv (Just Introduction) Stage1_1 (Just Stage1_2) Stage1.exercise1
              FrontendRoute_Stage1_2 -> setEnv (Just Stage1_1) Stage1_2 (Just Stage1_3) Stage1.exercise2
              FrontendRoute_Stage1_3 -> setEnv (Just Stage1_2) Stage1_3 (Just Stage1_4) Stage1.exercise3
              FrontendRoute_Stage1_4 -> setEnv (Just Stage1_3) Stage1_4 (Just Stage1_5) Stage1.exercise4
              FrontendRoute_Stage1_5 -> setEnv (Just Stage1_4) Stage1_5 (Just Stage1_6) Stage1.exercise5
              FrontendRoute_Stage1_6 -> setEnv (Just Stage1_5) Stage1_6 (Just Stage1_7) Stage1.exercise6
              FrontendRoute_Stage1_7 -> setEnv (Just Stage1_6) Stage1_7 (Just Stage2_1) Stage1.exercise7
              FrontendRoute_Stage2_1 -> setEnv (Just Stage1_7) Stage2_1 (Just Stage2_2) Stage2.exercise1
              FrontendRoute_Stage2_2 -> setEnv (Just Stage2_1) Stage2_2 Nothing         Stage2.exercise2
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
