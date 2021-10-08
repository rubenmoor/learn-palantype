{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Page.Stage2 where

import           Common.Alphabet        (PTChord, showChord, ulfts)
import           Common.Route           (FrontendRoute (..))
import           Control.Applicative    (Applicative (pure), (<$>))
import           Control.Category       (Category (id, (.)))
import           Control.Lens           ((.~), (<&>))
import           Control.Monad          (when, (=<<))
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.Reader   (MonadReader (ask))
import           Data.Bool              (Bool (..))
import           Data.Eq                (Eq ((==)))
import           Data.Foldable          (Foldable (length), for_)
import           Data.Function          (($))
import           Data.Functor           (void)
import           Data.Int               (Int)
import           Data.List              (zip, (!!))
import           Data.Maybe             (Maybe (..))
import           Data.Ord               (Ord ((>)))
import           Data.Semigroup         (Endo)
import qualified Data.Text              as Text
import           Data.Witherable        (Filterable (catMaybes, filter))
import           GHC.Num                (Num ((+), (-)))
import           Obelisk.Route.Frontend (R, RouteToUrl, SetRoute (setRoute))
import           Page.Common            (elCongraz,
                                         parseStenoOrError)
import           Reflex.Dom             (DomBuilder, EventWriter,
                                         MonadHold (holdDyn),
                                         PostBuild (getPostBuild), Prerender,
                                         Reflex (Event, updated), blank, dyn_, el, elAttr, elClass,
                                         elDynClass, foldDyn, text, widgetHold_,
                                         (=:))
import           State                  (Env (..),
                                         Navigation (..), State)

exercise1
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t (Endo State) m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Env t) m
  , PostBuild t m
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

    let steno = "TH CFIC P+RAUN FOCS +YUMPS OEFR TH LE^/+SI T+OC+ ^"
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

    eDone <- walkWords =<< parseStenoOrError steno

    elCongraz eDone envNavigation

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
      for_ (zip [0 :: Int ..] chords) $ \(i, c) -> do
        let dynCls = dynWalk <&> \WalkState {..} ->
              case wsMMistake of
                Just j  -> if i == j then "bgRed" else ""
                Nothing -> if wsCounter > i then "bgGreen" else ""
        elDynClass "span" dynCls $ text $ showChord c

  let eMistake = wsMMistake <$> updated dynWalk
  widgetHold_ blank $ eMistake <&> \case
      Just _ ->
        elClass "code" "blinking" $ text "ULFTS"
      Nothing -> blank

  dynDone <- holdDyn False eDone
  dyn_ $ dynDone <&> \bDone -> when bDone $
    elClass "div" "small anthrazit" $ text "Cleared. Press any key to start over."

  pure $ void $ filter id eDone
