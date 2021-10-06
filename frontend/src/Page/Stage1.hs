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
{-# LANGUAGE TypeApplications    #-}

module Page.Stage1 where

import           Common.Alphabet        (PTChar (..), PTChord (..), isLeftHand,
                                         isRightHand, showChord', showKey,
                                         showLetter)
import           Common.Api             (PloverCfg (pcfgMapStenoKeys))
import           Common.Route           (FrontendRoute (..))
import           Control.Applicative    (Applicative (pure), (<$>))
import           Control.Category       (Category (id, (.)))
import           Control.Lens           ((.~), (<&>))
import           Control.Monad          (when)
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Random   (evalRand, newStdGen)
import           Control.Monad.Reader   (MonadReader (ask), asks)
import           Data.Bool              (Bool (..))
import           Data.Eq                (Eq ((==)))
import           Data.Foldable          (Foldable (length), for_)
import           Data.Function          (($))
import           Data.Functor           (void, ($>))
import           Data.Generics.Product  (field)
import           Data.Int               (Int)
import           Data.List              (zip, (!!))
import qualified Data.Map               as Map
import           Data.Maybe             (Maybe (..))
import           Data.Ord               (Ord ((<), (>)))
import           Data.Semigroup         (Semigroup ((<>)))
import qualified Data.Text              as Text
import           Data.Witherable        (Filterable (catMaybes, filter))
import           GHC.Num                (Num ((+), (-)))
import           Obelisk.Route.Frontend (R, SetRoute)
import           Page.Common            (elCongraz)
import           Reflex.Dom             (DomBuilder, EventWriter,
                                         MonadHold (holdDyn),
                                         PostBuild (getPostBuild), Prerender,
                                         Reflex (Dynamic, Event, updated),
                                         blank, dynText, dyn_, el, elClass,
                                         elDynClass, foldDyn, performEvent,
                                         text, widgetHold_)
import           Shared                 (dynSimple, prerenderSimple,
                                         widgetHoldSimple)
import           State                  (EStateUpdate, Env (..),
                                         Navigation (..), State (..),
                                         updateState)
import           System.Random.Shuffle  (shuffleM)
import           Text.Show              (Show (show))

-- exercise 1

data WalkState = WalkState
  { wsCounter  :: Int
  , wsMMistake :: Maybe (Int, PTChord)
  , wsDone     :: Maybe Bool
  }

exercise1
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Env t) m
  , PostBuild t m
  , SetRoute t (R FrontendRoute) m
  )
  => m Navigation
exercise1 = do

  Env {..} <- ask

  el "h1" $ text "Stage 1"
  el "h2" $ text "The Palantype Alphabet"
  el "h3" $ text "Exercise 1"
  elClass "div" "paragraph" $
    text
      "Palantype relies on chords. A chord means: \
      \You press up to ten keys at the same time. \
      \The order in which you press down does not matter. \
      \Instead, all the letters of one chord will appear in palan order. \
      \Therefore, you will learn the Palantype Alphabet in its proper order now."
  elClass "div" "paragraph" $
    text "Type the following steno letters in order, one after another."
  elClass "div" "paragraph" $
    text
      "Some letters occur twice, the first time for your left hand \
      \and the second time for your right hand."

  ePb <- getPostBuild
  updateState $ ePb $> (field @"stShowKeyboard" .~ True)

  eDone <- taskAlphabet True
  elCongraz eDone envNavigation
  pure envNavigation

-- 1.2

exercise2
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Env t) m
  , PostBuild t m
  , SetRoute t (R FrontendRoute) m
  )
  => m Navigation
exercise2 = do

  Env {..} <- ask

  el "h1" $ text "Stage 1"
  el "h2" $ text "The Palantype Alphabet"
  el "h3" $ text "Exercise 2"
  el "span" $
    text
      "Again, type the letters in the Palantype Alphabet. \
      \But now, without seeing them. \
      \Learn to remember the correct order by \
      \pronouncing each letter while you type it!"

  ePb <- getPostBuild
  updateState $ ePb $> (field @"stShowKeyboard" .~ True)

  eDone <- taskAlphabet False
  elCongraz eDone envNavigation
  pure envNavigation

-- 1.3

exercise3
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Env t) m
  , PostBuild t m
  , SetRoute t (R FrontendRoute) m
  )
  => m Navigation
exercise3 = do

  Env {..} <- ask

  el "h1" $ text "Stage 1"
  el "h2" $ text "The Palantype Alphabet"
  el "h3" $ text "Exercise 3"
  elClass "div" "paragraph" $
    text
      "How about you type the letters in palan order \
      \without the virtual keyboard? \
      \Again, get used to remembering them!"

  ePb <- getPostBuild
  updateState $ ePb $> (field @"stShowKeyboard" .~ False)

  eDone <- taskAlphabet True
  elCongraz eDone envNavigation
  pure envNavigation

-- 1.4

exercise4
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Env t) m
  , PostBuild t m
  , SetRoute t (R FrontendRoute) m
  )
  => m Navigation
exercise4 = do

  Env {..} <- ask

  el "h1" $ text "Stage 1"
  el "h2" $ text "The Palantype Alphabet"
  el "h3" $ text "Exercise 4"
  elClass "div" "paragraph" $
    text
      "And for maximum difficulty, type the letters in palan \
      \order without seeing neither the letters nor the keyboard!"

  ePb <- getPostBuild
  updateState $ ePb $> (field @"stShowKeyboard" .~ False)

  eDone <- taskAlphabet False
  elCongraz eDone envNavigation
  pure envNavigation

taskAlphabet
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Env t) m
  , PostBuild t m
  )
  => Bool
  -> m (Event t ())
taskAlphabet showAlphabet = do
  Env {..} <- ask

  let dynAlphabet =
        Map.keys
          . pcfgMapStenoKeys
          . stPloverCfg
          <$> envDynState

  dynSimple $ dynAlphabet <&> \ptAlphabet -> do
    let len = length ptAlphabet

        step :: PTChord -> WalkState -> WalkState
        step chord ws@WalkState {..} =
          case (unPTChord chord, wsMMistake, wsDone) of
            (_, _, Just True) -> ws { wsDone = Just False
                                    , wsCounter = 0
                                    } -- reset after done
            _ | wsCounter == len - 1 ->
                                 ws { wsDone = Just True
                                    , wsCounter = wsCounter + 1
                                    } -- done
            (_, Just _, _) ->    ws { wsCounter = 0
                                    , wsMMistake = Nothing
                                    } -- reset after mistake
            ([l], _, _) | ptAlphabet !! wsCounter == l ->
                                 ws { wsDone = Nothing
                                    , wsCounter = wsCounter + 1
                                    } -- correct
            (ls, _, _) ->        ws { wsDone = Nothing
                                    , wsMMistake = Just (wsCounter, PTChord ls)
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
          let dynCls =
                dynWalk <&> \WalkState {..} -> case wsMMistake of
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

-- stage 1.5

data StenoLettersState = StenoLettersState
  { slsCounter  :: Int
  , slsMMistake :: Maybe (Int, PTChord)
  , slsDone     :: Maybe Bool
  , slsLetters  :: [PTChar]
  }

taskLetters
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Env t) m
  , PostBuild t m
  , Prerender js t m
  )
  => Dynamic t [PTChar]
  -> m (Event t ())
taskLetters dynLetters = do

  eChord <- asks envEChord

  eStdGen <- prerenderSimple $ do
    ePb <- getPostBuild
    performEvent $ ePb $> liftIO newStdGen

  widgetHoldSimple $ eStdGen <&> \stdGen -> do
    dynSimple $ dynLetters <&> \letters -> mdo
      let len = length letters

          step :: PTChord -> StenoLettersState -> StenoLettersState
          step chord ls@StenoLettersState {..} =
            case (unPTChord chord, slsMMistake, slsDone) of
              (_, _, Just True) ->
                               let letters' =
                                     evalRand (shuffleM slsLetters) stdGen
                               in  ls { slsDone = Just False
                                      , slsCounter = 0
                                      , slsLetters = letters'
                                      } -- reset after done
              _ | slsCounter == len - 1 ->
                                   ls { slsDone = Just True
                                      , slsCounter = slsCounter + 1
                                      } -- done
              (_, Just _, _) ->    ls { slsCounter = 0
                                      , slsMMistake = Nothing
                                      } -- reset after mistake
              ([l], _, _) | slsLetters !! slsCounter == l ->
                                   ls { slsDone = Nothing
                                      , slsCounter = slsCounter + 1
                                      } -- correct
              (wrong, _, _) ->     ls { slsDone = Nothing
                                      , slsMMistake =
                                          Just (slsCounter, PTChord wrong)
                                      } -- mistake

          stepInitial =
            StenoLettersState
              { slsCounter = 0
              , slsMMistake = Nothing
              , slsDone = Nothing
              , slsLetters = evalRand (shuffleM letters) stdGen
              }

      dynStenoLetters <- foldDyn step stepInitial eChord

      let eDone = catMaybes $ slsDone <$> updated dynStenoLetters

      dyn_ $
        dynStenoLetters <&> \StenoLettersState {..} -> do
          let clsMistake = case slsMMistake of
                Nothing -> ""
                Just _  -> "bgRed"
          when (slsCounter < len) $
            el "pre" $ elClass "code" clsMistake $
              text $ showKey $ slsLetters !! slsCounter
          el "span" $ do
            el "strong" $ text (Text.pack $ show slsCounter)
            text " / "
            text (Text.pack $ show len)

      let eMMistake = slsMMistake <$> updated dynStenoLetters
      widgetHold_ blank $
        eMMistake <&> \case
          Just (_, chord) ->
            elClass "div" "red small" $ text $
              "You typed " <> showChord' chord <> ". Any key to start over."
          Nothing -> blank

      dynDone <- holdDyn False eDone
      dyn_ $ dynDone <&> \bDone -> when bDone $
        elClass "div" "small anthrazit" $
          text "Cleared. Press any key to start over."

      pure $ void $ filter id eDone

exercise5
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Env t) m
  , Prerender js t m
  , PostBuild t m
  , SetRoute t (R FrontendRoute) m
  )
  => m Navigation
exercise5 = do

  Env {..} <- ask

  el "h1" $ text "Stage 1"
  el "h2" $ text "The Palantype Alphabet"
  el "h3" $ text "Exercise 5"
  elClass "div" "paragraph" $ do
    text "You get the virtual keyboard back. Feel free, to toggle it anytime. \
         \You can even use "
    el "code" $ text "STFL"
    text "to do that. This is a chord that just exists \
         \for this purpose here on this website. \
         \It doesn't have a meaning, so it hopefully doesn't interfere."
  elClass "div" "paragraph" $
    text "Type every steno letter as it appears!"
  elClass "div" "paragraph" $ do
    text "The - symbol is used to distinguish between letters that appear \
         \twice. In this task, you will only need your left hand. Thus \
         \some letters have a trailing -."

  ePb <- getPostBuild
  updateState $ ePb $> (field @"stShowKeyboard" .~ True)

  let dynLeftHand
        =   filter isLeftHand
          . Map.keys
          . pcfgMapStenoKeys
          . stPloverCfg
        <$> envDynState

  eDone <- taskLetters dynLeftHand

  elCongraz eDone envNavigation
  pure envNavigation

exercise6
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Env t) m
  , Prerender js t m
  , PostBuild t m
  , SetRoute t (R FrontendRoute) m
  ) => m Navigation
exercise6 = do

  Env {..} <- ask

  el "h1" $ text "Stage 1"
  el "h2" $ text "The Palantype Alphabet"
  el "h3" $ text "Exercise 6"
  elClass "div" "paragraph" $ do
    text "Switching hands now. The leading - symbol indicates that the letter \
         \is on your right-hand side."
  elClass "div" "paragraph" $ do
    text "Type every steno letter as it appears!"

  ePb <- getPostBuild
  updateState $ ePb $> (field @"stShowKeyboard" .~ True)

  let dynRightHand
        =   filter isRightHand
          . Map.keys
          . pcfgMapStenoKeys
          . stPloverCfg
        <$> envDynState

  eDone <- taskLetters dynRightHand

  elCongraz eDone envNavigation
  pure envNavigation

exercise7
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Env t) m
  , Prerender js t m
  , PostBuild t m
  , SetRoute t (R FrontendRoute) m
  ) => m Navigation
exercise7 = do

  Env {..} <- ask

  el "h1" $ text "Stage 1"
  el "h2" $ text "The Palantype Alphabet"
  el "h3" $ text "Exercise 7"
  elClass "div" "paragraph" $ do
    text "Before your continue with this last exercise of Stage 1: There is a \
         \table of contents on the left. Use it to jump back to any of the \
         \previous exercises to practice some more."
  elClass "div" "paragraph" $ do
    text "For the next stage, you should have some muscle memory for every \
         \key. Be sure to complete this exercise without the keyboard, too."
  elClass "div" "paragraph" $ do
    text "Type every steno letter as it appears!"

  ePb <- getPostBuild
  updateState $ ePb $> (field @"stShowKeyboard" .~ True)

  let dynAlphabet
        =   Map.keys
          . pcfgMapStenoKeys
          . stPloverCfg
        <$> envDynState

  eDone <- taskLetters dynAlphabet

  elCongraz eDone envNavigation
  pure envNavigation
