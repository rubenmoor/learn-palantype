{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Page.Stage2 where

import           Common.Route           (FrontendRoute (..))
import           Control.Applicative    (Applicative (pure))
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.Reader   (MonadReader (ask))
import           Obelisk.Route.Frontend (R, RouteToUrl,
                                         SetRoute (setRoute))
import           Reflex.Dom             (Reflex(Event), (=:), elAttr, elClass, text, el, DomBuilder, EventWriter,
                                         MonadHold (holdDyn),
                                         PostBuild (getPostBuild), Prerender)
import           State                  (EStateUpdate, Env (..),
                                         Navigation (..))
import Data.Function (($))
import qualified Data.Text as Text
import Data.Foldable (Foldable(length), for_)
import Control.Category (Category((.)))
import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..))
import qualified Data.Map as Map
import Common.Alphabet (ulfts, PTChord(unPTChord, PTChord))
import Data.Int (Int)
import Text.Read (read)
import GHC.Num (Num((+), (-)))
import Data.Eq (Eq((==)))
import Data.List ((!!))

exercise1
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Env t) m
  , Prerender js t m
  , PostBuild t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  )
  => m Navigation
exercise1 = do
  Env {..} <- ask
  el "h1" $ text "Stage 2"
  el "h2" $ text "Syllables and chords"
  el "h3" $ text "Exercise 1"
  elClass "div" "paragraph" $ do
    text "We can begin with actually typing sentences now. \
         \How about this one sentences I found in the "
    elAttr "a" ("href" =: "http://www.openstenoproject.org/palantype/tutorial/2016/08/21/learn-palantype.html")
      $ text "Palantype Tutorial"
    text " of the Open Steno Project:"

    let steno = "-TH CFIC HRAUN FOCS +YUMPS OEFR TH LE^/+SI T+OC+ ^"
        txt = "The quick brown fox jumps over the lazy dog ."

    el "blockquote" $
      el "table" $ do
        el "tr" $
          for_ (Text.words txt)   $ el "td" . text
        el "tr" $
          for_ (Text.words steno) $ el "td" . el "pre" . el "code" . text

    elClass "div" "paragraph" $ do
      text "Each word is one chord, except the word \"lazy\". You will \
           \ have to strike "
      el "code" $ text "LE^"
      text " and "
      el "code" $ text "+SI"
      text " separately. This is what the /-symbol means."

    elClass "div" "paragraph" $ do
      text "Let me introduce yet another useful chord: "
      el "code" $ text "ULFTS"
      text ". It is the homerow of your right hand and deletes your last \
           \input. Now you can correct your mistakes!"

  pure envNavigation

data WalkState = WalkState
  { wsCounter  :: Int
  , wsMMistake :: Maybe Int
  , wsDone     :: Maybe Bool
  }

walkWords
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Env t) m
  , PostBuild t m
  )
  => [PTChord]
  -> m (Event t ())
walkWords chords = do
  Env {..} <- ask

  let len = length chords

      step :: PTChord -> WalkState -> WalkState
      step chord ws@WalkState {..} =
        case (wsMMistake, wsDone) of
          (_, Just True) -> ws { wsDone = Just False
                               , wsCounter = 0
                               } -- reset after done
          _ | wsCounter == len - 1 ->
                            ws { wsDone = Just True
                               , wsCounter = wsCounter + 1
                               } -- done
          (Just _, _) | chord == ulfts ->
                            ws { wsMMistake = Nothing
                               , wsCounter = wsCounter - 1
                               }  -- undo stroke
          (Just _, _) ->    ws   -- halt while mistake
          (_, _) | chords !! wsCounter == chord ->
                            ws { wsDone = Nothing
                               , wsCounter = wsCounter + 1
                               } -- correct
          (_, _) ->         ws { wsDone = Nothing
                               , wsMMistake = Just wsCounter
                               } -- mistake

      stepInitial = WalkState
          { wsCounter = 0
          , wsMMistake = Nothing
          , wsDone = Nothing
          }

  dynWalk <- foldDyn step stepInitial envEChord
  let eDone = catMaybes $ wsDone <$> updated dynWalk

  el "pre" $
    el "code" $ do
      let clsLetter = if showAlphabet then "" else "fgTransparent"
      for_ (zip [0 :: Int ..] ptAlphabet) $ \(i, c) -> do
        let dynCls = dynWalk <&> \WalkState {..} ->
              case wsMMistake of
                Just (j, _) -> if i == j then "bgRed" else clsLetter
                Nothing -> if wsCounter > i then "bgGreen" else clsLetter
        elDynClass "span" dynCls $ text $ showLetter c
  el "span" $ do
    dynText $ dynWalk <&> \WalkState {..} -> Text.pack $ show wsCounter
    text $ " / " <> Text.pack (show len)

  let eMistake = wsMMistake <$> updated dynWalk
  widgetHold_ blank $
    eMistake <&> \case
      Just (_, w) ->
        elClass "div" "red small" $
          text $
            "You typed " <> showChord' w
              <> ". Any key to start over."
      Nothing -> blank

  dynDone <- holdDyn False eDone
  dyn_ $
    dynDone <&> \bDone ->
      when bDone $
        elClass "div" "small anthrazit" $ text "Cleared. Press any key to start over."

  pure $ void $ filter id eDone
