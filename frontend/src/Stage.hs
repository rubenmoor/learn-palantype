{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Stage where

import           Common.Alphabet        (showChord, PTChord(..),  PTChar, showKey)
import           Common.Api             (PloverCfg (pcfgMapStenoKeys))
import           Common.Route           (FrontendRoute (..))
import           Control.Applicative    (Applicative (pure), (<$>))
import           Control.Category       (Category(id, (.)))
import           Control.Lens           ((.~), (<&>))
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.Random   (mkStdGen, evalRand, StdGen, MonadRandom)
import           Control.Monad.Reader   (withReaderT, MonadReader (ask))
import           Data.Bool              (Bool (..), bool)
import           Data.Eq                (Eq ((==)))
import           Data.Foldable          (Foldable (length), for_)
import           Data.Function          (($))
import           Data.Functor           (Functor(fmap), ($>), void)
import           Data.Generics.Product  (field)
import           Data.Int               (Int)
import           Data.List              (take, replicate, zip, (!!))
import qualified Data.Map               as Map
import           Data.Maybe             (Maybe (..))
import           Data.Monoid            (Monoid (mconcat))
import           Data.Ord               (Ord ((>)))
import           Data.Semigroup         (Semigroup ((<>)))
import           Data.Set               (Set)
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import           Data.Witherable        (Filterable(filter, catMaybes))
import           GHC.Num                (Num ((+), (-)))
import           Obelisk.Route.Frontend (mapRoutedT, pattern (:/), R, RouteToUrl,
                                         SetRoute (setRoute), routeLink)
import           Reflex.Dom             (tailE, dynText, DomBuilder, EventName (Click),
                                         EventWriter, HasDomEvent (domEvent),
                                         MonadHold(hold, holdDyn),
                                         PostBuild (getPostBuild), Prerender,
                                         Reflex (Dynamic, Event, never, updated),
                                         blank, dyn, dyn_, el, el',
                                         elClass, elDynClass,
                                         foldDyn, leftmost,
                                         switchHold, text, widgetHold_)
import           Shared                 (iFa)
import           State                  (EStateUpdate, Stage (..), State (..),
                                         stageUrl, updateState)
import           System.Random.Shuffle  (shuffleM)
import Text.Show (Show(show))
import Data.Tuple (snd)

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
    routeLink (stageUrl nxt) $ text $ Text.pack $ show nxt
    text " by completing the tasks or skip "
    -- TODO: define chords to navigate
    elClass "code" "chord" $ text "ASDF"
    text "."

-- 1.1

data WalkState = WalkState
  { wsCounter  :: Int
  , wsMMistake :: Maybe (Int, PTChord)
  , wsDone     :: Bool
  }

stage1_1
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Dynamic t State, Event t PTChord) m
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
  el "div" $ text "Type the following steno letters in order, one after another."
  el "div" $ text "Some letters occur twice, the first time for your left hand \
                   \and the second time for your right hand."

  taskAlphabet True Stage1_2

stage1_2
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Dynamic t State, Event t PTChord) m
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
  , MonadReader (Dynamic t State, Event t PTChord) m
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
  , MonadReader (Dynamic t State, Event t PTChord) m
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
  , MonadReader (Dynamic t State, Event t PTChord) m
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

  let dynAlphabet = take 3 .Map.keys . pcfgMapStenoKeys . stPloverCfg <$> dynState
  dyn_ $ dynAlphabet <&> \ptAlphabet -> do

    let len = length ptAlphabet

        step :: PTChord -> WalkState -> WalkState
        step w ws@WalkState{..} = case (unPTChord w, wsMMistake, wsDone) of
          (_, _, True)         -> ws { wsDone = False
                                     -- , wsCounter = 0
                                     }                    -- reset after done
          _ | wsCounter == len - 1
                               -> ws { wsDone = True }    -- done
          (_, Just _, _)       -> ws { wsCounter = 0
                                     , wsMMistake = Nothing
                                     }                    -- reset after mistake
          ([l], _, _) | ptAlphabet !! wsCounter == l ->
                                  ws { wsCounter = wsCounter + 1
                                     }                    -- correct
          (ls,  _, _)          -> ws { wsMMistake =
                                         Just (wsCounter, PTChord ls)
                                     }                    -- mistake

        stepInitial = WalkState
          { wsCounter = 0
          , wsMMistake = Nothing
          , wsDone = False
          }
    dynWalk <- foldDyn step stepInitial eWord
    let eDone
          = filter id
          $ updated
          $ wsDone <$> dynWalk

    el "pre" $ el "code" $ do
      let clsLetter = if showAlphabet then "" else "fgTransparent"
      for_ (zip [0 :: Int ..] ptAlphabet) $ \(i, c) -> do
        let dynCls = dynWalk <&> \WalkState{..} -> case wsMMistake of
              Just (j, _) -> if i == j then "bgRed" else clsLetter
              Nothing     -> if wsCounter > i then "bgGreen" else clsLetter
        elDynClass "span" dynCls $ text $ showKey c
    el "span" $ do
      dynText $ dynWalk <&> \WalkState{..} -> Text.pack $ show wsCounter
      text $ " / " <> Text.pack (show len)

    let eMistake = wsMMistake <$> updated dynWalk
    widgetHold_ blank $ eMistake <&> \case
      Just (_, w)  -> elClass "div" "red small" $ text
         $ "You typed " <> showChord w
        <> ". Any key to start over."
      Nothing -> blank

    updateState $ eDone $> (field @"stProgress" .~ nxt)
    elCongraz eDone

  elFooterNextStage nxt

data StenoLettersState = StenoLettersState
  { slsCounter :: Int
  , slsMMistake :: Maybe (Int, PTChord)
  , slsDone    :: Bool
  }

stage1_5
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Dynamic t State, Event t PTChord) m
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
    let rndGen = mkStdGen 0
        lsLetters = take 3 $ evalRand (shuffleM $ mconcat $ replicate 2 ptAlphabet) rndGen
        len = length lsLetters

        step :: PTChord -> StenoLettersState -> StenoLettersState
        step chord ls@StenoLettersState{..} =
          case (unPTChord chord, slsMMistake, slsDone) of
            (_, _, True)          -> ls { slsDone = False
                                        , slsCounter = 0
                                        }                   -- reset after done
            _ | slsCounter == len - 1
                                  -> ls { slsDone = True }  -- done
            (_, Just _, _)        -> ls { slsCounter = 0
                                        , slsMMistake = Nothing
                                        }                   -- reset after mistake
            ([l], _, _) | lsLetters !! slsCounter == l ->
                                     ls { slsCounter = slsCounter + 1
                                        }                   -- correct
            (wrong,  _, _)        -> ls { slsMMistake =
                                            Just (slsCounter, PTChord wrong)
                                        }                   -- mistake

        stepInitial = StenoLettersState
          { slsCounter = 0
          , slsMMistake = Nothing
          , slsDone = False
          }

    dynStenoLetters <- foldDyn step stepInitial eWord

    let eDone = filter id
              $ updated
              $ slsDone <$> dynStenoLetters

    dyn_ $ dynStenoLetters <&> \StenoLettersState{..} -> do
      let clsMistake= case slsMMistake of
            Nothing -> ""
            Just _  -> "bgRed"
      elClass "code" clsMistake $
        text $ showKey $ lsLetters !! slsCounter
      el "span" $ do
        el "strong" $ text (Text.pack $ show slsCounter)
        text " / "
        text (Text.pack $ show len)

    let eMMistake = slsMMistake <$> updated dynStenoLetters
    widgetHold_ blank $ eMMistake <&> \case
      Just (_, chord)  -> elClass "div" "red small" $ text
         $ "You typed " <> showChord chord
        <> ". Any key to start over."
      Nothing -> blank

    updateState $ eDone $> (field @"stProgress" .~ Stage2_1)
    elCongraz eDone

  elFooterNextStage Stage2_1

elCongraz
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Dynamic t State, Event t PTChord) m
  , PostBuild t m
  , SetRoute t (R FrontendRoute) m
  )
  => Event t ()
  -> m ()
elCongraz eDone = mdo
  (_, eWord) <- ask
  dynShowCongraz <- holdDyn False $ leftmost [eDone $> True, eBack $> False]
  eBack <- switchHold never eEBack
  eEBack <- dyn $ dynShowCongraz <&> \case
    False -> pure never
    True  ->
      elClass "div" "mkOverlay" $ elClass "div" "congraz" $ do
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

stage2_1
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Dynamic t State, Event t PTChord) m
  , Prerender js t m
  , PostBuild t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  )
  => m ()
stage2_1 = blank
