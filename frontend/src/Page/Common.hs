{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Page.Common where

import Client (postRender)
import Common.Route (FrontendRoute (..))
import Common.Stage (stageMeta)
import Control.Applicative (Applicative (pure))
import Control.Category ((<<<), Category (id, (.)))
import Control.Lens
    (preview, (?~), (%~),
      (.~),
      (<&>),
    )
import Control.Monad (when)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Random (evalRand, newStdGen)
import Control.Monad.Reader
    ( MonadReader,
      asks,
    )
import Data.Bool (bool, not, Bool (..))
import Data.Either (Either (..))
import Data.Eq (Eq ((==)))
import Data.Foldable (for_, traverse_)
import Data.Foldable (Foldable (null))
import Data.Function (($))
import Data.Functor
    ( ($>),
      void,
    )
import Data.Functor ((<$>))
import Data.Generics.Product (field)
import Data.Int (Int)
import Data.List ((!!))
import Data.List (intersperse)
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid (mempty))
import Data.Ord ((<), Ord ((>)))
import Data.Semigroup
    ( Endo (..),
      Semigroup ((<>)),
    )
import qualified Data.Set as Set
import Data.Text (Text, toLower)
import Data.Text (length)
import Data.Witherable (Filterable (filter))
import GHC.Enum (Enum (succ), pred)
import GHC.Float (Double)
import GHC.Num ((*), (-), fromInteger, Num ((+)))
import GHC.Real (div, floor, realToFrac, (/), fromIntegral)
import Obelisk.Route.Frontend
    ( R,
      RouteToUrl,
      SetRoute (setRoute),
      routeLink,
      pattern (:/),
    )
import Palantype.Common
    (kiChordsStart,  Chord,
      Lang (..),
      Palantype,
      PatternPos,
      unparts,
    )
import Palantype.Common
    ( RawSteno,
      parseSteno,
    )
import Palantype.Common (kiBackUp, kiEnter)
import qualified Palantype.Common.Indices as KI
import Reflex.Dom
    (tickLossyFromPostBuildTime, dynText, TriggerEvent, tickLossyFrom', Performable, PerformEvent, widgetHold, switchDyn, holdUniqDyn, (=:),
      DomBuilder, TickInfo (..),
      EventName (Click),
      EventWriter,
      HasDomEvent (domEvent),
      MonadHold (holdDyn),
      PostBuild (getPostBuild),
      Prerender,
      Reflex (Event, never),
      blank,
      dyn_,
      el,
      elAttr,
      elClass,
      elClass',
      foldDyn,
      leftmost,
      performEvent,
      text,
      updated,
    )
import Safe (initMay)
import Shared
    ( dynSimple,
      iFa,
      whenJust,
    )
import State
    ( Env (..),
      Message (..),
      Navigation (..),
      State,
      stageUrl,
      updateState,
    )
import System.Random.Shuffle (shuffleM)
import TextShow (showt)
import Text.Read (read)
import Control.Lens (makePrisms, makeLenses)
import qualified Palantype.Common.RawSteno as Raw
import Data.Function ((&))
import Data.Functor (Functor(fmap))
import Data.Foldable (Foldable(elem))
import Data.Time (diffUTCTime, UTCTime, NominalDiffTime, getCurrentTime)
import qualified Data.Text as Text
import Text.Printf (printf)
import Palantype.Common.TH (failure)

elFooter ::
    forall t (m :: * -> *).
    ( DomBuilder t m,
      Prerender t m,
      RouteToUrl (R FrontendRoute) m,
      SetRoute t (R FrontendRoute) m
    ) =>
    Lang ->
    Navigation ->
    m ()
elFooter lang Navigation {..} = elClass "footer" "stage" $ do
    whenJust navMPrevious $ \prv -> do
        elClass "div" "floatLeft" $ do
            text "< "
            routeLink (stageUrl lang prv) $ text $ showt $ stageMeta prv
    text $ showt $ stageMeta navCurrent
    whenJust navMNext $ \nxt -> do
        elClass "div" "floatRight" $ do
            routeLink (stageUrl lang nxt) $ text $ showt $ stageMeta nxt
            text " >"
    elClass "br" "clearBoth" blank

elBackUp ::
    forall key (m :: * -> *) t.
    ( DomBuilder t m,
      Palantype key
    ) =>
    m ()
elBackUp =
    elClass "span" "btnSteno"
        $ text
        $ "↤ " <> showt (KI.toRaw @key kiBackUp) -- U+21A4

elCongraz ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      Palantype key,
      PostBuild t m,
      SetRoute t (R FrontendRoute) m
    ) =>
    Event t () ->
    Navigation ->
    m ()
elCongraz eDone Navigation {..} = mdo
    eChord <- asks envEChord
    let eChordEnter = void $ filter (\c -> KI.fromChord c == kiEnter) eChord
        eChordBackUp = void $ filter (\c -> KI.fromChord c == kiBackUp) eChord

    dynShowCongraz <- holdDyn False $ leftmost [eDone $> True, eBack $> False]
    eBack <- dynSimple $
        dynShowCongraz <&> \case
            False -> pure never
            True -> elClass "div" "mkOverlay" $ elClass "div" "congraz" $ do
                el "div" $ text "Task cleared!"
                el "div" $ iFa "fas fa-check-circle"
                whenJust navMNext $ \nxt -> do
                    elACont <- elClass "div" "anthrazit" $ do
                        text "Type "
                        elClass "span" "btnSteno" $ do
                            el "em" $ text "Enter "
                            el "code" $ text $ showt $ KI.toRaw @key kiEnter
                        text " to continue to "
                        elClass "div" "paragraph" $ do
                            (e, _) <- elClass' "a" "normalLink" $ text $ showt $ stageMeta nxt
                            text "."
                            pure e
                    let eContinue = leftmost [eChordEnter, domEvent Click elACont]
                    updateState $
                        eContinue
                            $> [ field @"stProgress"
                                     %~ Map.update
                                         (\s -> if nxt > s then Just nxt else Just s)
                                         navLang,
                                 field @"stCleared" %~ Set.insert navCurrent,
                                 if nxt == read "stage_2-1"
                                     then field @"stTOCShowStage2" .~ True
                                     else id
                               ]
                    setRoute $ eContinue $> FrontendRoute_Main :/ ()
                el "div" $ do
                    el "span" $ text "("
                    (elABack, _) <- elClass' "a" "normalLink" $ text "back"
                    text " "
                    elBackUp @key
                    el "span" $ text ")"
                    pure $ leftmost [eChordBackUp, domEvent Click elABack]
    blank

parseStenoOrError ::
    forall proxy key t (m :: * -> *).
    (EventWriter t (Endo State) m, Palantype key, PostBuild t m) =>
    proxy key ->
    RawSteno ->
    m (Maybe [Chord key])
parseStenoOrError _ raw = case parseSteno raw of
    Right words -> pure $ Just words
    Left err -> do
        ePb <- getPostBuild
        let msgCaption = "Internal error"
            msgBody =
                "Could not parse steno code: " <> showt raw <> "\n" <> err
        updateState $ ePb $> [field @"stMsg" .~ Just Message {..}]
        pure Nothing

elNotImplemented :: forall (m :: * -> *) t. DomBuilder t m => m ()
elNotImplemented = elClass "blockquote" "warning" $ do
    el "strong" $ text "Not implemented"
    el "br" blank
    text
        "You are currently looking at an exercise that has not been \
        \implemented for the original English palantype. \
        \Feel free to read, but don't expect things to work from here on."

rawToggleKeyboard :: Lang -> RawSteno
rawToggleKeyboard = \case
    DE -> "ULNSD"
    EN -> "ALFTS"

loading ::
    forall (m :: * -> *) t.
    ( DomBuilder t m
    ) =>
    m ()
loading =
    elClass "div" "paragraph" $ do
        iFa "fas fa-spinner fa-spin"
        text " Loading ..."

data StateWords = StateWords
    { _stDone :: Bool
    , _stStep :: Step
    }

data Step
    = StepPause
    | StepRun Run

data Run = Run
    { _stCounter   :: Int
    , _stChords    :: [RawSteno]
    , _stWords     :: [Text]
    , _stNMistakes :: Int
    , _stMHint     :: Maybe [RawSteno]
    }

makeLenses ''StateWords
makePrisms ''Step
makeLenses ''Run

taskWords
  :: forall key t (m :: * -> *)
  . ( DomBuilder t m
    , MonadFix m
    , MonadHold t m
    , MonadIO (Performable m)
    , MonadReader (Env t key) m
    , Palantype key
    , PerformEvent t m
    , PostBuild t m
    , Prerender t m
    , TriggerEvent t m
    )
  => Map RawSteno Text
  -> Map Text [RawSteno]
  -> m (Event t ())
taskWords mapStenoWord mapWordStenos = do
    eChord <- asks envEChord

    evStdGen <- postRender $ do
        ePb <- getPostBuild
        performEvent $ ePb $> liftIO newStdGen

    fmap switchDyn $ widgetHold (loading $> never) $ evStdGen <&> \stdGen -> do
        let
            len = Map.size mapWordStenos

            step :: Chord key -> StateWords -> StateWords
            step c st = case st of
                StateWords _ StepPause ->
                    if Raw.fromChord c `elem` (KI.toRaw @key <$> kiChordsStart)
                        then stepStart
                        else st
                -- undo last input
                StateWords _ (StepRun Run{..})
                    | Raw.fromChord c == KI.toRaw @key kiBackUp ->
                        case initMay _stChords of
                           Just cs -> st & stStep . _StepRun %~
                                 ( stChords .~ cs )
                               . ( stNMistakes %~ succ )
                               . ( stMHint .~ Nothing )
                           Nothing -> st & stStep . _StepRun . stMHint ?~
                               ( Map.findWithDefault []
                                     (_stWords !! _stCounter)
                                     mapWordStenos
                               )
                StateWords _ (StepRun Run{..}) ->
                    let rawSteno = unparts $ _stChords <> [Raw.fromChord c]
                        word = Map.findWithDefault "" rawSteno mapStenoWord
                    in  if word == _stWords !! _stCounter

                            -- correct
                            then if _stCounter == pred len
                                then st & stDone .~ True
                                        & stStep .~ StepPause
                                else st & stStep . _StepRun %~
                                      ( stCounter %~ succ )
                                    . ( stMHint .~ Nothing )
                                    . ( stChords .~ [] )

                            -- not correct
                            else st & stStep
                                    . _StepRun
                                    . stChords
                                    .~ (_stChords <> [Raw.fromChord c])
            stepStart = StateWords
                { _stDone = False
                , _stStep = StepRun Run
                    { _stCounter = 0
                    , _stChords = []
                    , _stWords =
                        evalRand (shuffleM $ Map.keys mapWordStenos) stdGen
                    , _stNMistakes = 0
                    , _stMHint = Nothing
                    }
                }

            stateInitial = StateWords
                { _stDone = False
                , _stStep = StepPause
                }

        dynStateWords <- foldDyn step stateInitial eChord

        eDone <- fmap updated $ holdUniqDyn $ _stDone <$> dynStateWords

        elClass "div" "taskWords" $ dyn_ $ dynStateWords <&> \case
            StateWords _ StepPause -> el "div" $ do
                text "Type "
                elClass "span" "btnSteno blinking" $ do
                    text "Start "
                    el "code" $ text "SDAÜD"
                text " to begin the exercise."
            StateWords _ (StepRun Run{..}) -> when (_stCounter < len) $ do
                -- elClass "span" "exerciseField"
                elClass "span" "word"
                    $ elClass "div" "exerciseField"
                    $ el "code" $ text $ _stWords !! _stCounter

                elClass "span" "input" $
                    for_ ( intersperse "/" $ (showt <$> _stChords) <> [" …"]
                         ) $ \str -> el "code" $ text str

                el "span" $ do
                    elClass "span" "btnSteno" $ text $
                        "↤ " <> showt (KI.toRaw @key kiBackUp) -- U+21A4
                    elClass "span" "small" $ text $
                        if null _stChords
                            then " to show hint"
                            else " to back up"

                whenJust _stMHint $ \hint ->
                    elClass "div" "small" $ for_ hint $ \r -> do
                        text $ showt r
                        el "br" blank

        let dynCounter =
                dynStateWords <&> preview (stStep . _StepRun . stCounter)
        dyn_ $ dynCounter <&> \mC -> whenJust mC \c ->
            elClass "div" "paragraph" $ do
                el "strong" $ text $ showt c
                text " / "
                text $ showt len

        evStartStop <- fmap updated $ holdUniqDyn $ dynStateWords <&> \case
            StateWords _ StepPause   -> False
            StateWords _ (StepRun _) -> True
        evToggle <- performEvent $ evStartStop $> (ESWToggle <$> liftIO getCurrentTime)
        evTick <- fmap (ESWTick <<< _tickInfo_lastUTC) <$> tickLossyFromPostBuildTime 0.1

        let startStop :: EventStopwatch -> StateStopwatch -> StateStopwatch
            startStop (ESWTick timeTick  ) (SWRun timeStart _) =
                SWRun timeStart $ diffUTCTime timeTick timeStart
            startStop (ESWToggle timeStop) (SWRun timeStart _) =
                SWStop $ diffUTCTime timeStop timeStart
            startStop (ESWToggle timeStart) _ = SWRun timeStart 0

        dynStopWatch <- foldDyn startStop SWInitial $ leftmost [evToggle, evTick]

        pure $ void $ filter id eDone
  where
    -- in time 1.8.0.2 there is not FormatTime instance for Difftime
    -- (or NominalDifftime)
    -- and GHC 8.6.5 depends on that one specifically
    formatTime :: NominalDiffTime -> Text
    formatTime dt =
        let seconds = realToFrac dt
            secondsFull = floor @Double @Int seconds
            secondsTenth = floor @Double @Int $ (seconds - fromIntegral secondsFull) * 10
            minutes = secondsFull `div` 60
            strMinutes = if minutes > 0 then printf "%1d:" minutes else ""
            strSeconds = printf "%1d." secondsFull
        in  Text.pack (strMinutes <> strSeconds) <> showt secondsTenth

data StateStopwatch
    = SWInitial
    | SWRun UTCTime NominalDiffTime
    | SWStop  NominalDiffTime

data EventStopwatch
    = ESWToggle UTCTime
    | ESWTick UTCTime

elPatterns ::
    forall (m :: * -> *) t.
    DomBuilder t m =>
    [(PatternPos, [(Text, RawSteno)])] ->
    m ()
elPatterns doc =
    elClass "div" "patternTable" $ traverse_ elPatterns' doc
    where
        elPatterns' (pPos, pairs) = do
            let strPPos = toLower $ showt pPos
            elClass "hr" strPPos blank
            elClass "span" ("patternPosition " <> strPPos) $ text strPPos
            elClass "br" "clearBoth" blank
            for_ pairs $ \(orig, steno) ->
                elClass "div" "floatLeft" $ do
                    let lOrig :: Double = fromIntegral $ length orig
                        styleOrig =
                            if lOrig > 6
                                then "style" =: ("font-size: " <> showt ((1 + 6 / lOrig) / 2) <> "em")
                                else mempty
                        lSteno :: Double = fromIntegral $ length $ showt steno
                        styleSteno =
                            if lSteno > 6
                                then "style" =: ("font-size: " <> showt (6 / lSteno) <> "em")
                                else mempty
                    elAttr "div" ("class" =: "orig" <> styleOrig) $ text orig
                    elAttr "code" ("class" =: "steno" <> styleSteno) $ text $ showt steno
            elClass "br" "clearBoth" blank
