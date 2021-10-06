{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Stage where

import           Common.Alphabet        (PTChar (..), PTChord (..), isLeftHand,
                                         isRightHand, mkPTChord, showChord',
                                         showKey, showLetter)
import           Common.Api             (PloverCfg (pcfgMapStenoKeys))
import           Common.Route           (FrontendRoute (..))
import           Control.Applicative    (Applicative (pure), (<$>))
import           Control.Category       (Category (id, (.)))
import           Control.Lens           ((%~), (.~), (<&>))
import           Control.Monad          (when)
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Random   (evalRand, newStdGen)
import           Control.Monad.Reader   (MonadReader (ask), asks)
import           Data.Bool              (Bool (..))
import           Data.Eq                (Eq ((==)))
import           Data.Foldable          (Foldable (length), for_)
import           Data.Function          (($))
import           Data.Functor           (Functor (fmap), void, ($>))
import           Data.Generics.Product  (field)
import           Data.Int               (Int)
import           Data.List              (zip, (!!))
import qualified Data.Map               as Map
import           Data.Maybe             (Maybe (..))
import           Data.Ord               (Ord ((>)))
import           Data.Semigroup         (Endo(..), Semigroup ((<>)))
import qualified Data.Text              as Text
import           Data.Witherable        (Filterable (catMaybes, filter))
import           GHC.Num                (Num ((+), (-)))
import           Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl,
                                         SetRoute (setRoute), routeLink)
import           Reflex.Dom             (DomBuilder, EventName (Click),
                                         EventWriter, HasDomEvent (domEvent),
                                         MonadHold (holdDyn),
                                         PostBuild (getPostBuild),
                                         Reflex (Dynamic, Event, never, updated), blank,
                                         dynText, dyn_, el, elAttr, elClass,
                                         elClass', elDynClass, foldDyn,
                                         leftmost, performEvent, switchDyn,
                                         text, widgetHold, widgetHold_,
                                         (=:), Prerender)
import           Shared                 (if', dynSimple, iFa, loadingScreen,
                                         prerenderSimple, whenJust)
import           State                  (EStateUpdate, Env (..), Stage (..),
                                         State (..), stageUrl, updateState, Navigation (..))
import           System.Random.Shuffle  (shuffleM)
import           Text.Show              (Show (show))
import Data.Monoid (Monoid(mconcat))
import qualified Data.Set as Set

elFooter
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , Prerender js t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  )
  => Navigation
  -> m ()
elFooter Navigation{..} = el "footer" $ do
  whenJust navMPrevious $ \prv -> do
    elClass "div" "floatLeft" $ do
      text "< "
      routeLink (stageUrl prv) $ text $ Text.pack $ show prv
  text $ Text.pack $ show navCurrent
  whenJust navMNext $ \nxt -> do
    elClass "div" "floatRight" $ do
      routeLink (stageUrl nxt) $ text $ Text.pack $ show nxt
      text " >"
  elClass "br" "clearBoth" blank

elCongraz
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Env t) m
  , PostBuild t m
  , SetRoute t (R FrontendRoute) m
  )
  => Event t ()
  -> Navigation
  -> m ()
elCongraz eDone Navigation{..} = mdo

  eChord <- asks envEChord

  let chordCON = mkPTChord [LeftC, LeftO, RightN]
      chordBACK = mkPTChord [LeftP, LeftCross, RightA, RightC]
      eChordCON = void $ filter (== chordCON) eChord
      eChordBACK = void $ filter (== chordBACK) eChord

  dynShowCongraz <- holdDyn False $ leftmost [eDone $> True, eBack $> False]
  eBack <- dynSimple $ dynShowCongraz <&> \case
    False -> pure never
    True ->
      elClass "div" "mkOverlay" $
        elClass "div" "congraz" $ do
          el "div" $ text "Task cleared!"
          el "div" $ iFa "fas fa-check-circle"
          whenJust navMNext $ \nxt -> do
            (elACont, _) <- elClass "div" "anthrazit" $ do
              text "Type "
              el "code" $ text "CON"
              text " to continue to "
              elClass' "a" "normalLink" (text $ Text.pack $ show nxt)
            let eContinue = leftmost [eChordCON, domEvent Click elACont]
            updateState $ eContinue $> appEndo (mconcat
              [ Endo $ field @"stProgress" %~ \s -> if nxt > s then nxt else s
              , Endo $ field @"stCleared" %~ Set.insert navCurrent
              , if' (nxt == Stage2_1) $
                  Endo $ field @"stTOCShowStage2" .~ True
              ])
            setRoute $ eContinue $> FrontendRoute_Main :/ ()
          el "div" $ do
            el "span" $ text "("
            (elABack, _) <- elClass' "a" "normalLink" $ text "back"
            text " "
            el "code" $ text "P+AC"
            el "span" $ text ")"
            pure $ leftmost [eChordBACK, domEvent Click elABack]
  blank

-- introduction

introduction
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadReader (Env t) m
  , SetRoute t (R FrontendRoute) m
  )
  => m Navigation
introduction = do

  Env{..} <- ask

  el "h1" $ text "Introduction"
  el "h2" $ text "Why Palantype"
  elClass "div" "paragraph" $
    text
      "Palantype allows you to type with lightning speed. \
      \It's a stenographic system in the wider sense. \
      \The most widespread of these systems is simply called steno. \
      \Any steno-style system requires quite a bit of learning. \
      \Palantype has the advantage that it's more suitable for regular \
      \keyboards. There are limitations, though."
  el "h2" $ text "Requirements"
  el "h3" $ text "Hardware"
  elClass "div" "paragraph" $
    text
      "You can get started with your regular keyboard. \
      \However, keyboards usually have limit of how many keys register \
      \at the same time. Long term, you will need a keyboard that supports \
      \N-Key roll-over (NKR) to type chords of up to ten keys. \
      \In addition, a ortholinear layout as well as very sensitive keys \
      \are preferable."
  elClass "div" "paragraph" $
    text
      "You can play around with the keyboard above to see how much keys \
      \register at the same time with your hardware."

  el "h3" $ text "Software"
  elClass "div" "paragraph" $ do
    text "All of this is made possible by the "
    elAttr "a" ("href" =: "http://www.openstenoproject.org/") $ text "Open Steno Project"
    text ". The software "
    elAttr "a" ("href" =: "http://www.openstenoproject.org/plover/") $ text "Plover"
    text
      " is part of the project and is all you need to get serious. \
      \As long as you practice here, you don't need Plover. \
      \Once you installed and configured Plover however, you can upload your \
      \Plover configuration here to practice with the same key map."
  elClass "div" "paragraph" $ do
    text "Be sure to check out additional information on "
    elAttr "a" ("href" =: "http://www.openstenoproject.org/palantype/tutorial/2016/08/21/learn-palantype.html") $ text "learning Palantype"
    text " and the "
    elAttr "a" ("href" =: "http://www.openstenoproject.org/palantype/palantype/2016/08/21/palan-versus-steno.html") $ text "differences between Palantype and Stenography"
    text "."

  let chordSTART = mkPTChord [LeftS, LeftT, RightA, RightR, RightT]
      eChordSTART = void $ filter (== chordSTART) envEChord

  elClass "div" "start" $ do
    (btn, _) <- elClass' "button" "start" $ text "Get Started!"
    let eStart = leftmost [eChordSTART, domEvent Click btn]
    updateState $ eStart $> appEndo (mconcat
      [ Endo $ field @"stProgress" .~ Stage1_1
      , Endo $ field @"stCleared" %~ Set.insert (navCurrent envNavigation)
      , Endo $ field @"stTOCShowStage1" .~ True
      ])
    setRoute $ eStart $> FrontendRoute_Main :/ ()

  elClass "div" "paragraph" $ do
    text "Instead of clicking the button, try to input "
    el "code" $ text "START"
    text " by pressing S-, T-, A, -R, and -T all at once. Take your time \
         \finding the next key while holding down. The chord is only registered \
         \once you release all the keys."
  pure envNavigation

-- 1.1

data WalkState = WalkState
  { wsCounter  :: Int
  , wsMMistake :: Maybe (Int, PTChord)
  , wsDone     :: Maybe Bool
  }

stage1_1
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
stage1_1 = do

  Env {..} <- ask

  el "h1" $ text "Stage 1"
  el "h2" $ text "The Palantype Alphabet"
  el "h3" $ text "Task 1"
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

stage1_2
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
stage1_2 = do

  Env {..} <- ask

  el "h1" $ text "Stage 1"
  el "h2" $ text "The Palantype Alphabet"
  el "h3" $ text "Task 2"
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

stage1_3
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
stage1_3 = do

  Env {..} <- ask

  el "h1" $ text "Stage 1"
  el "h2" $ text "The Palantype Alphabet"
  el "h3" $ text "Task 3"
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

stage1_4
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
stage1_4 = do

  Env {..} <- ask

  el "h1" $ text "Stage 1"
  el "h2" $ text "The Palantype Alphabet"
  el "h3" $ text "Task 4"
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

  dynSimple $
    dynAlphabet <&> \ptAlphabet -> do
      let len = length ptAlphabet

          step :: PTChord -> WalkState -> WalkState
          step w ws@WalkState {..} = case (unPTChord w, wsMMistake, wsDone) of
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

  fmap switchDyn $ widgetHold (loadingScreen $> never) $ eStdGen <&> \stdGen -> do
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

      let eDone = catMaybes $ updated $ slsDone <$> dynStenoLetters

      dyn_ $
        dynStenoLetters <&> \StenoLettersState {..} -> do
          let clsMistake = case slsMMistake of
                Nothing -> ""
                Just _  -> "bgRed"
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

stage1_5
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
stage1_5 = do

  Env {..} <- ask

  el "h1" $ text "Stage 1"
  el "h2" $ text "The Palantype Alphabet"
  el "h3" $ text "Task 5"
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

stage1_6
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
stage1_6 = do

  Env {..} <- ask

  el "h1" $ text "Stage 1"
  el "h2" $ text "The Palantype Alphabet"
  el "h3" $ text "Task 6"
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

stage1_7
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
stage1_7 = do

  Env {..} <- ask

  el "h1" $ text "Stage 1"
  el "h2" $ text "The Palantype Alphabet"
  el "h3" $ text "Task 7"
  elClass "div" "paragraph" $ do
    text "Before your continue with this last task of Stage 1: There is a \
         \table of contents on the left. Use it to jump back to any of the \
         \previous exercises to practice some more."
  elClass "div" "paragraph" $ do
    text "For the next stage, you should have some muscle memory for every \
         \key. Be sure to complete this task without the keyboard, too."
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

stage2_1
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
stage2_1 = do
  Env {..} <- ask
  pure envNavigation
