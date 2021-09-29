{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Stage where

import           Common.Alphabet        (PTChar, showKey)
import           Common.Api             (PloverCfg (pcfgMapStenoKeys))
import           Common.Route           (FrontendRoute (..))
import           Control.Applicative    (Applicative(pure), (<$>))
import           Control.Category       (Category ((.)))
import           Control.Lens           ((.~), (<&>))
import           Control.Monad          (when)
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.Reader   (Functor (fmap), MonadReader (ask),
                                         MonadTrans (lift))
import           Data.Bool              (Bool (..))
import           Data.Eq                (Eq ((==)))
import           Data.Foldable          (Foldable (length), for_)
import           Data.Function          (($))
import           Data.Functor           (($>))
import           Data.Generics.Product  (field)
import           Data.Int               (Int)
import           Data.List              (replicate, sort, zip, (!!))
import qualified Data.Map               as Map
import           Data.Maybe             (Maybe (..))
import           Data.Ord               (Ord ((>)))
import           Data.Semigroup         (Semigroup ((<>)))
import           Data.Set               (Set)
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import           Data.Witherable        (Filterable (catMaybes))
import           GHC.Num                (Num ((+), (-)))
import           Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl,
                                         SetRoute (setRoute), routeLink)
import           Reflex.Dom             (switchHold, switchDyn, dyn, EventName(Click), HasDomEvent(domEvent), el', DomBuilder, EventWriter,
                                         MonadHold (holdDyn), PostBuild(getPostBuild),
                                         Prerender,
                                         Reflex(never, Dynamic, Event, updated),
                                         blank, dynText, dyn_, el, elClass,
                                         elDynAttr, elDynClass, foldDyn,
                                         leftmost, text, widgetHold_)
import           Shared                 (whenJust, iFa)
import           State                  (EStateUpdate, Stage (..), State (..),
                                         stageUrl, updateState)
import Data.Monoid (Monoid(mconcat))
import Control.Monad.Random (MonadRandom)
import System.Random.Shuffle (shuffleM)
import Data.Functor (void)

elFooterNextStage
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , Prerender js t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  )
  => Stage
  -> m ()
elFooterNextStage nxt = do
  el "hr" blank
  el "div" $ do
    text "Advance to "
    routeLink (stageUrl nxt) $ text "Stage 1.2"
    text " by completing the tasks or skip "
    -- TODO: define chords to navigate
    elClass "code" "chord" $ text "ASDF"
    text "."

-- 1.1

data WalkState = WalkState
  { wsCounter  :: Int
  , wsMMistake :: Maybe (Int, Set PTChar)
  , wsDone     :: Bool
  }

stage1_1
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Dynamic t State, Event t (Set PTChar)) m
  , Prerender js t m
  , PostBuild t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  )
  => m ()
stage1_1 = do
  el "h1" $ text "Stage 1"
  el "h2" $ text "The stenographic alphabet"
  el "h3" $ text "Task 1"
  el "span" $ text "Type the following steno letters in order, one after another:"


  el "span" $ text "Some letters occur twice, the first time for your left hand \
                   \and the second time for your right hand."

  taskAlphabet True Stage1_2

stage1_2
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Dynamic t State, Event t (Set PTChar)) m
  , Prerender js t m
  , PostBuild t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  )
  => m ()
stage1_2 = do
  el "h1" $ text "Stage 1"
  el "h2" $ text "The stenographic alphabet"
  el "h3" $ text "Task 2"
  el "span" $ text "Again, type the letters in the palan order. \
                   \But now, without seeing them. \
                   \Learn to remember the correct order by \
                   \pronouncing each letter while you type it!"

  taskAlphabet False Stage1_3

stage1_3
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Dynamic t State, Event t (Set PTChar)) m
  , Prerender js t m
  , PostBuild t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  )
  => m ()
stage1_3 = do
  el "h1" $ text "Stage 1"
  el "h2" $ text "The stenographic alphabet"
  el "h3" $ text "Task 3"
  el "span" $ text "How about you type the letters in palan order \
                   \without the virtual keyboard? \
                   \Again, get used to remembering them!"
  ePb <- getPostBuild
  updateState $ ePb $> (field @"stShowKeyboard" .~ False)
  taskAlphabet True Stage1_4

stage1_4
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Dynamic t State, Event t (Set PTChar)) m
  , Prerender js t m
  , PostBuild t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  )
  => m ()
stage1_4 = do
  el "h1" $ text "Stage 1"
  el "h2" $ text "The stenographic alphabet"
  el "h3" $ text "Task 4"
  el "span" $ text "And for maximum difficulty, type the letters in palan \
                   \order without seeing neither the letters nor the keyboard!"

  taskAlphabet False Stage1_5

taskAlphabet
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Dynamic t State, Event t (Set PTChar)) m
  , Prerender js t m
  , PostBuild t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  )
  => Bool
  -> Stage
  -> m ()
taskAlphabet showAlphabet nxt = do

  (dynState, eWord) <- ask

  let dynAlphabet = Map.keys . pcfgMapStenoKeys . stPloverCfg <$> dynState
  dyn_ $ dynAlphabet <&> \ptAlphabet -> do

    let len = length ptAlphabet - 1
        step :: Set PTChar -> WalkState -> WalkState
        step w ws@WalkState{..} = case (Set.toList w, wsMMistake, wsDone) of
          (_, _, True)         -> ws { wsDone = False
                                     , wsCounter = 0
                                     }                        -- done
          _ | wsCounter == len -> ws { wsDone = True }
          (_, Just _, _)       -> ws { wsCounter = 0
                                     , wsMMistake = Nothing
                                     }                        -- reset
          ([l], _, _) | ptAlphabet !! wsCounter == l ->
                                  ws { wsCounter = wsCounter + 1
                                     }                        -- correct
          (ls,  _, _)          -> ws { wsMMistake =
                                         Just ( wsCounter , Set.fromList ls)
                                     }                        -- mistake

        stepInitial = WalkState
          { wsCounter = 0
          , wsMMistake = Nothing
          , wsDone = False
          }
    dynWalk <- foldDyn step stepInitial eWord
    let eDone = catMaybes $ updated $ dynWalk <&> \ws ->
          if wsDone ws then Just () else Nothing

    el "pre" $ el "code" $ do
      let clsLetter = if showAlphabet then "" else "fgTransparent"
      for_ (zip [0 :: Int ..] ptAlphabet) $ \(i, c) -> do
        let dynCls = dynWalk <&> \WalkState{..} -> case wsMMistake of
              Just (j, _) -> if i == j then "bgRed" else clsLetter
              Nothing     -> if wsCounter > i then "bgGreen" else clsLetter
        elDynClass "span" dynCls $ text $ showKey c

    widgetHold_ blank $ updated dynWalk <&> \WalkState{..} -> case wsMMistake of
      Just (_, w)  -> elClass "div" "red small" $ text
        -- TODO: function to properly show steno words
         $ "You typed " <> Text.unwords (showKey <$> Set.toList w)
        <> ". Any key to start over."
      Nothing -> blank

    updateState $ eDone $> (field @"stProgress" .~ nxt)
    elCongraz (void eWord) eDone

  elFooterNextStage nxt

data LettersState = LettersState
  { lsCounter :: Int
  , lsMMistake :: Maybe (Int, Set PTChar)
  , lsDone :: Bool
  }

stage1_5
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , MonadRandom m
  , MonadReader (Dynamic t State, Event t (Set PTChar)) m
  , Prerender js t m
  , PostBuild t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  )
  => m ()
stage1_5 = do
  el "h1" $ text "Stage 1"
  el "h2" $ text "The stenographic alphabet"
  el "h3" $ text "Task 5"
  el "div" $ text "You get the virtual keyboard back. \
                   \Feel free, to toggle it anytime. \
                   \You can even use ASDF to do that.\
                   \This is a chord that just exists for this purpose here on \
                   \this website."
  el "div" $ text "In order to be prepared for Stage 2, you should be able \
                  \to do this task without the virtual keyboard."
  el "div" $ text "Type every steno letter as it appears!"

  ePb <- getPostBuild
  updateState $ ePb $> (field @"stShowKeyboard" .~ True)

  (dynState, eWord) <- ask

  let dynAlphabet = Map.keys . pcfgMapStenoKeys . stPloverCfg <$> dynState
  dyn_ $ dynAlphabet <&> \ptAlphabet -> mdo
    lsLetters <- shuffleM $ mconcat $ replicate 2 ptAlphabet
    let len = length lsLetters

        step :: Set PTChar -> LettersState -> LettersState
        step w ls@LettersState{..} = case (Set.toList w, lsMMistake, lsDone) of
          (_, _, True)         -> ls { lsDone = False
                                     , lsCounter = 0
                                     }                        -- done
          _ | wsCounter == len -> ls { lsDone = True }
          (_, Just _, _)       -> ls { lsCounter = 0
                                     , lsMMistake = Nothing
                                     }                        -- reset
          ([l], _, _) | lsLetters !! lsCounter == l ->
                                  ls { lsCounter = lsCounter + 1
                                     }                        -- correct
          (wrong,  _, _)       -> ls { lsMMistake =
                                         Just ( lsCounter , Set.fromList wrong)
                                     }                        -- mistake

        stepInitial = LettersState
          { lsCounter = 0
          , lsMMistake = Nothing
          , lsDone = False
          }
    dynLetter <- foldDyn step stepInitial eWord
    let eDone = catMaybes $ updated $ dynLetter <&> \ls ->
          if lsDone ls then Just () else Nothing

    dyn_ $ dynLetter <&> \LettersState{..} -> case lsMMistake of
      Nothing     -> do
        el "code" $ text $ showKey $ lsLetters !! lsCounter
      Just (j, _) -> blank

    widgetHold_ blank $ updated dynLetter <&> \LettersState{..} -> case lsMMistake of
      Just (_, w)  -> elClass "div" "red small" $ text
        -- TODO: function to properly show steno words
         $ "You typed " <> Text.unwords (showKey <$> Set.toList w)
        <> ". Any key to start over."
      Nothing -> blank

    updateState $ eDone $> (field @"stProgress" .~ Stage2_1)
    elCongraz (void eWord) eDone

  elFooterNextStage Stage2_1

elCongraz
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Dynamic t State, Event t (Set PTChar)) m
  , Prerender js t m
  , PostBuild t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  )
  => Event t ()
  -> Event t ()
  -> m ()
elCongraz eWord eDone = mdo
  dynShowCongraz <- holdDyn False $ leftmost [eDone $> True, eBack $> False]
  eBack <- switchHold never eEBack
  eEBack <- dyn $ dynShowCongraz <&> \case
    False -> pure never
    True  -> elClass "div" "mkOverlay" $ elClass "div" "congraz" $ do
      el "div" $ text "Task cleared!"
      el "div" $ iFa "fas fa-check-circle"
      el "div" $ text "Press any key to continue"
      setRoute $ eWord $> FrontendRoute_Main :/ ()
      el "div" $ do
        el "span" $ text "("
        (elBack, _) <- el' "a" $ text "back"
        el "span" $ text ")"
        pure $ domEvent Click elBack
  blank
