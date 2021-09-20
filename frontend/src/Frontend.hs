{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
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
import           Common.Alphabet             (PTChar (..))
import           Common.Api                  (PloverCfg (..))
import           Common.Route                (FrontendRoute)
import           Control.Monad               (unless, (<=<))
import           Data.Foldable               (for_)
import           Data.Function               ((&))
import           Data.Functor                (void)
import qualified Data.Map                    as Map
import           Data.Maybe                  (mapMaybe, isNothing, listToMaybe)
import qualified Data.Text                   as Text
import           Data.Witherable             (Filterable (catMaybes))
import           GHCJS.DOM.FileReader        (getResult, load, newFileReader,
                                              readAsText)
import           JSDOM.EventM                (on)
import           JSDOM.Types                 (File)
import           Obelisk.Route.Frontend      (RoutedT)
import           Reflex.Dom                  (elClass, DomBuilder (inputElement),
                                              InputElement (..),
                                              MonadHold (holdDyn),
                                              PerformEvent (performEvent_),
                                              Reflex (Event, updated), def,
                                              elementConfig_initialAttributes,
                                              ffor, fmapMaybe,
                                              inputElementConfig_elementConfig,
                                              widgetHold_, wrapDomEvent, (.~))
import           Servant.Common.Req          (ReqResult (..))
import           Text.Read                   (readMaybe)
import Data.List (partition)
import Data.Set (Set)
import qualified Data.Set as Set

-- fileInput :: DomBuilder t m => m (Dynamic t [File])

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
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

    eReqResult <- postRender $ do
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
  let stenoKeys = Map.keys pcfgStenoKeys
      ls = zip stenoKeys $ readMaybe <$> stenoKeys
      unrecognized = mapMaybe (\(f, s) ->
        if isNothing s && f /= "no-op" && f /= "arpeggiate"
          then Just f
          else Nothing) ls
      recognized = mapMaybe snd ls
  unless (null unrecognized) $ void $ el "div" $ do
    text "Your key map contains unrecognized entries: "
    for_ unrecognized $ \s -> el "div" $ text $ Text.pack s
  elPTKeyboard $ Set.fromList recognized

elPTKeyboard
  :: DomBuilder t m
  => Set PTChar
  -> m ()
elPTKeyboard ptChars = elClass "table" "keyboard" $ do
    el "tr" $ do
      elCell LeftC "1"
      elCell LeftP "1"
      elCell LeftM "1"
      elCell LeftN "1"
      elAttr "td" ("colspan" =: "4" <> "class" =: "gap") blank
      elCell RightN "1"
      elCell RightM "1"
      elCell RightP "1"
      elCell RightH "1"
    el "tr" $ do
      elCell LeftS "1"
      elCell LeftT "1"
      elCell LeftF "1"
      elCell LeftL "1"
      elAttr "td" ("colspan" =: "3" <> "class" =: "gap") blank
      elCell RightE "1"
      elCell RightL "1"
      elCell RightF "1"
      elCell RightT "1"
      elCell RightS "1"
    el "tr" $ do
      elCell LeftCross "1"
      elCell LeftH "1"
      elCell LeftR "1"
      elCell LeftY "1"
      elCell LeftO "1"
      elCell MiddleI "2"
      elCell RightA "1"
      elCell RightC "1"
      elCell RightR "1"
      elCell RightCross "1"
      elCell RightPoint "1"
    el "tr" $ do
      elAttr "td" ("colspan" =: "4" <> "class" =: "gap") blank
      elCell LeftE "1"
      elCell LeftPipe "1"
      elCell RightPipe "1"
      elCell RightU "1"
  where
    elCell c colspan =
      if Set.member c ptChars
        then elAttr "td" ("colspan" =: colspan) $ text $ Text.pack $ show c
        else elAttr "td" ("colspan" =: colspan <> "class" =: "gap") blank
