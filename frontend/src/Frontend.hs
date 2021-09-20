{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Frontend where

import           Language.Javascript.JSaddle (FromJSVal (fromJSVal),
                                              ToJSVal (toJSVal), liftJSM)

import           Obelisk.Frontend            (Frontend (..), ObeliskWidget)
import           Obelisk.Generated.Static    (static)
import           Obelisk.Route               (R)

import           Reflex.Dom.Core             (blank, el, elAttr, text, (=:))

import           Client                      (postConfigNew, postRender)
import           Common.Route                (FrontendRoute)
import           Control.Monad               ((<=<))
import           Data.Function               ((&))
import           Data.Functor                (void)
import           Data.Maybe                  (listToMaybe)
import qualified Data.Text                   as Text
import           Data.Witherable             (Filterable (catMaybes))
import           GHCJS.DOM.FileReader        (getResult, load, newFileReader,
                                              readAsText)
import           JSDOM.EventM                (on)
import           JSDOM.Types                 (File)
import           Obelisk.Route.Frontend      (RoutedT)
import           Reflex.Dom                  (DomBuilder (DomBuilderSpace, inputElement),
                                              InputElement (..),
                                              MonadHold (holdDyn),
                                              PerformEvent (performEvent_),
                                              Prerender (prerender),
                                              Reflex(Event, Dynamic, never, updated),
                                              def, dyn_,
                                              elementConfig_initialAttributes,
                                              ffor, fmapMaybe,
                                              inputElementConfig_elementConfig,
                                              switchDyn, widgetHold,
                                              widgetHold_, wrapDomEvent, (.~))
import           Servant.Common.Req          (ReqResult (..))
import Common.Api (PloverCfg (..))

-- fileInput :: DomBuilder t m => m (Dynamic t [File])

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Palantype ftw"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = frontendBody
  }

elFileInput
  :: DomBuilder t m
  => m (Event t File)
elFileInput = do
  i <- inputElement $ def
         & inputElementConfig_elementConfig
         . elementConfig_initialAttributes
         .~ ("type" =: "file" <> "accept" =: "text/cfg")

  let eFiles = _inputElement_files i
  pure $ fmapMaybe listToMaybe $ updated eFiles

frontendBody
  :: forall t js (m :: * -> *).
  ( ObeliskWidget  js t (R FrontendRoute) m
  )
  => RoutedT t (R FrontendRoute) m ()
frontendBody = do
  el "div" $ do
    el "h3" $ text "Upload Plover config"

    eFile <- elFileInput

    eReqResult <- fmap switchDyn $ prerender (pure never) $ do
      fileReader <- liftJSM newFileReader
      let encoding = Just ("utf8" :: String)
      performEvent_ $ fmap (\f -> readAsText fileReader (Just f) encoding) eFile
      eText <- fmap catMaybes $ wrapDomEvent fileReader (`on` load) $ liftJSM $ do
        v <- getResult fileReader
        (fromJSVal <=< toJSVal) v
      dynEitherText <- holdDyn (Left "no file") (Right . Text.unpack <$> eText)
      postConfigNew dynEitherText (void eText)

    widgetHold_ blank $ ffor eReqResult $ \case
      ResponseSuccess _ cfg _ -> elPloverCfg cfg
      ResponseFailure _ msg _ -> el "span" $ text msg
      RequestFailure _ msg -> el "span" $ text msg
  pure ()

elPloverCfg
  :: DomBuilder t m
  => PloverCfg
  -> m ()
elPloverCfg PloverCfg{..} = el "div" $ do
  el "h4" $ text "System"
  el "span" $ text pcfgSystem
  el "h4" $ text "Machine"
  el "span" $ text pcfgMachine
  el "div" $ text "Key map loaded."
