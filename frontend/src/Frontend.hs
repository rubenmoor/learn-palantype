{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Frontend where

import           Language.Javascript.JSaddle (FromJSVal (fromJSVal),
                                              ToJSVal (toJSVal), liftJSM)
import           State                       (EStateUpdate (..))

import           Obelisk.Frontend            (Frontend (..), ObeliskWidget)
import           Obelisk.Generated.Static    (static)
import           Obelisk.Route               (R)

import           Reflex.Dom.Core             (blank, el, elAttr, text, (=:))

import           Client                      (postConfigNew, postRender)
import           Common.Alphabet             (PTChar (..), showKey)
import           Common.Api                  (PloverCfg (..))
import           Common.Keys                 (fromPlover)
import           Common.Route                (FrontendRoute)
import           Control.Monad               (unless, (<=<))
import           Control.Monad.Fix           (MonadFix)
import qualified Data.Aeson                  as Aeson
import           Data.Foldable               (for_)
import           Data.Function               ((&))
import           Data.Functor                (void, ($>))
import           Data.List                   (sort)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe                  (isNothing, listToMaybe, mapMaybe)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Lazy              as Lazy
import qualified Data.Text.Lazy.Encoding     as Lazy
import           Data.Witherable             (Filterable (catMaybes))
import           GHCJS.DOM                   (currentWindowUnchecked)
import           GHCJS.DOM.FileReader        (getResult, load, newFileReader,
                                              readAsText)
import           GHCJS.DOM.Storage           (getItem, setItem)
import           GHCJS.DOM.Window            (getLocalStorage)
import           JSDOM.EventM                (on)
import           JSDOM.Types                 (File)
import           Obelisk.Route.Frontend      (RoutedT)
import           Reflex.Dom                  (DomBuilder (DomBuilderSpace, inputElement),
                                              EventResult, InputElement (..),
                                              MonadHold (holdDyn),
                                              PerformEvent (performEvent_),
                                              PostBuild, Prerender (prerender),
                                              Reflex (Dynamic, Event, updated),
                                              def, dynText, dyn_, elClass,
                                              elDynAttr,
                                              elementConfig_initialAttributes,
                                              ffor, fmapMaybe, foldDyn,
                                              inputElementConfig_elementConfig,
                                              keydown, keyup, mergeWith,
                                              runEventWriterT, widgetHold_,
                                              wrapDomEvent, (.~))
import           Servant.Common.Req          (ReqResult (..))
import           Text.Read                   (readMaybe)

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
frontendBody = do

  let key = "state" :: Text
      getState s = getItem s key
      setState d s = setItem s key d

  dynLoadState <- prerender (pure $ EstateUpdate $ const def) $ do
    mStr <- liftJSM (currentWindowUnchecked >>= getLocalStorage >>= getState)
    let mState = mStr >>= Aeson.decode . Lazy.encodeUtf8. Lazy.fromStrict
    pure $ EstateUpdate $ const $ fromMaybe def mState

  let eLoaded = updated dynLoadState
  widgetHold_ loadingScreen $ eLoaded $> blank

  dynState <- foldDyn unEStateUpdate def $ leftmost [eLoaded, eStateUpdate]

  -- TODO: persist application state on visibility change (when hidden)
  eUpdated <- tailE $ updated dynState
  prerender_ blank $ performEvent_ $ ffor eUpdated $ \st -> do
    let str = Lazy.toStrict $ Lazy.decodeUtf8 $ Aeson.encode st
    liftJSM (currentWindowUnchecked >>= getLocalStorage >>= setState str)

  (_, eStateUpdate) <- mapRoutedT (flip runReaderT dynState . runEventWriterT)
    frontendHome
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
