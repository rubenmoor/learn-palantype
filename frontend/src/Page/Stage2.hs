{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Page.Stage2 where

import Client
    ( getDictDE',
      getDocDEPattern',
      postRender,
      request,
    )
import Common.Route (FrontendRoute (..))
import Control.Applicative
    ( (<$>),
      Applicative (pure),
    )
import Control.Category (Category ((.), id), (<<<))
import Control.Lens
    (preview, (+~), (?~), (%~), (.~),
      (<&>)
    )
import Control.Lens.TH
import Control.Monad
    ( (=<<),
      unless,
      when,
    )
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Random (evalRand)
import Control.Monad.Reader
    ( MonadReader (ask),
      asks,
    )
import Data.Bool (Bool (..))
import Data.Either (Either (..))
import Data.Eq (Eq ((==)))
import Data.Foldable
    ( Foldable (length),
      for_,
      traverse_,
    )
import Data.Function (($))
import Data.Functor
    ( ($>),
      void,
    )
import Data.Functor (Functor ((<$)))
import Data.Functor (Functor (fmap))
import Data.Generics.Product (field)
import Data.Int (Int)
import Data.List
    ( (!!),
      elem,
      zip,
    )
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Maybe (Maybe (..))
import Data.Ord (Ord ((<), (>)))
import Data.Semigroup
    ( (<>),
      Endo,
    )
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Witherable
    ( Filterable
          ( catMaybes,
            filter
          ),
    )
import GHC.Num (Num ((+), (-)))
import Obelisk.Route.Frontend
    ( R,
      RouteToUrl,
      SetRoute (setRoute),
      routeLink,
      pattern (:/),
    )
import Page.Common
    ( elBackUp,
      elCongraz,
      elNotImplemented,
      elPatterns,
      loading,
      taskWords,
    )
import Palantype.Common
    (kiChordsStart,  Chord (..),
      Lang (..),
      Palantype,
      fromChord,
    )
import Palantype.Common (kiBackUp, kiEnter)
import Palantype.Common (RawSteno, parseStenoMaybe)
import qualified Palantype.Common.Indices as KI
import Palantype.DE (Pattern (..))
import Reflex.Dom
    (TriggerEvent, Performable, holdUniqDyn,  (=:),
      DomBuilder,
      EventName (Click),
      EventWriter,
      HasDomEvent (domEvent),
      MonadHold (holdDyn),
      PerformEvent (performEvent),
      PostBuild (getPostBuild),
      Prerender,
      Reflex (Event, never, updated),
      blank,
      delay,
      dyn_,
      el,
      elAttr,
      elClass,
      elClass',
      elDynClass,
      filterRight,
      foldDyn,
      leftmost,
      switchDyn,
      text,
      widgetHold,
      widgetHold_,
      zipDyn,
    )
import Shared
    (whenJust)
import State
    ( Env (..),
      Navigation (..),
      State (..),
      stageUrl,
      updateState,
    )
import System.Random (newStdGen)
import System.Random.Shuffle (shuffleM)
import Text.Read (readMaybe)
import TextShow (TextShow (showt))
import Palantype.Common.TH (fromJust, readLoc)
import qualified Palantype.Common.RawSteno as Raw
import Data.Function ((&))
import Control.Monad (join)
import Data.Bifunctor (Bifunctor(second))

-- Ex. 2.1

exercise1 ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadReader (Env t key) m,
      Palantype key,
      SetRoute t (R FrontendRoute) m
    ) =>
    m Navigation
exercise1 = do
    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang `elem` [DE, EN]) elNotImplemented

    el "h1" $ text "Stage 2"
    el "h2" $ text "Syllables and chords"
    el "h3" $ text "Exercise 1"
    elClass "div" "paragraph" $ do
        text
            "You probably have noticed that on the virtual keyboard some keys \
            \are highlighted blue. These keys are called home row. \
            \The idea is that there is a resting position for your hands, where \
            \every finger is placed on one key of home row."
    elClass "div" "paragraph" $ do
        text
            "From this resting position your fingers can find any key, without \
            \any need to look down on your keyboard, so your eyes can stay on \
            \the screen all the time."
    elClass "div" "paragraph" $ do
        text
            "In case you have been looking on your fingers during the exercises, \
            \it's probably a good idea to get used to home row now. \
            \Simply repeat Stage 1 until you can do all the exercises without \
            \looking down. Just orient yourself once in the beginning!"

    let eChordEnter = void $ filter (\c -> KI.fromChord c == kiEnter) envEChord
        eChordBackUp = void $ filter (\c -> KI.fromChord c == kiBackUp) envEChord

    elABack <- elClass "div" "paragraph" $ do
        text "Type "
        elBackUp @key
        text " to "
        (e, _) <- elClass' "a" "normalLink" $ text "go back to Exercise 1.1"
        text " to practice home row."
        pure e

    let eBack = leftmost [eChordBackUp, domEvent Click elABack]

    setRoute $ eBack $> stageUrl navLang ($readLoc "stage_1-1")
    updateState $
        eBack
            $> [field @"stProgress" %~ Map.update (\_ -> Just $ $readLoc "stage_1-1") navLang]

    whenJust navMNext $ \nxt -> do
        (elACont, _) <- elClass "div" "anthrazit" $ do
            text "Type "
            elClass "span" "btnSteno" $ do
                el "em" $ text "Enter "
                text $ showt (KI.toRaw @key kiEnter)
            text " to continue to "
            elClass' "a" "normalLink" $ text $ showt nxt
        let eContinue = leftmost [eChordEnter, domEvent Click elACont]
        updateState $
            eContinue
                $> [ field @"stProgress"
                         %~ Map.update
                             (\s -> if nxt > s then Just nxt else Just s)
                             navLang,
                     field @"stCleared" %~ Set.insert navCurrent
                   ]
        setRoute $ eContinue $> FrontendRoute_Main :/ ()

    pure envNavigation

-- Ex 2.2

data WalkState = WalkState
    { wsCounter :: Int,
      wsMMistake :: Maybe Int,
      wsDone :: Maybe Bool
    }

walkWords ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      Palantype key,
      PostBuild t m
    ) =>
    [Text] ->
    RawSteno ->
    m (Event t ())
walkWords words raw = do
    Env {..} <- ask
    let Navigation {..} = envNavigation

    let chords = $fromJust $ parseStenoMaybe raw
        len = length chords
        step :: Chord key -> WalkState -> WalkState
        step chord ws@WalkState {..} = case (wsMMistake, wsDone) of
            -- reset after done
            (_, Just True) -> ws {wsDone = Just False, wsCounter = 0}
            -- undo stroke
            _
                | fromChord chord == KI.toRaw @key kiBackUp ->
                    ws {wsMMistake = Nothing}
            -- halt while mistake
            (Just _, _) -> ws
            -- correct
            _
                | chords !! wsCounter == chord ->
                    let done =
                            if wsCounter == len - 1
                                then Just True -- done
                                else Nothing
                     in ws {wsDone = done, wsCounter = wsCounter + 1}
            -- mistake
            _ -> ws {wsDone = Nothing, wsMMistake = Just wsCounter}
        stepInitial =
            WalkState
                { wsCounter = 0,
                  wsMMistake = Nothing,
                  wsDone = Nothing
                }

    dynWalk <- foldDyn step stepInitial envEChord
    let eDone = catMaybes $ wsDone <$> updated dynWalk

    el "blockquote" $ el "table" $ do
        el "tr" $ traverse_ (el "td" . text) words
        el "tr" $ do
            for_ (zip [0 :: Int ..] chords) $ \(i, c) -> do
                let dynCls = dynWalk <&> \WalkState {..} -> case wsMMistake of
                        Just j -> if i == j then "bgRed" else ""
                        Nothing -> if wsCounter > i then "bgGreen" else ""
                el "td" $ elDynClass "span" dynCls $ el "code" $ text $ showt c

            el "td" $ do
                let eMistake = wsMMistake <$> updated dynWalk
                widgetHold_ blank $
                    eMistake <&> \case
                        Just _ -> elClass "code" "blinking" $ do
                            text " "
                            elBackUp @key
                        Nothing -> blank

    dynDone <- holdDyn False eDone
    dyn_ $
        dynDone <&> \bDone ->
            when bDone $ elClass "div" "small anthrazit" $
                text
                    "Cleared. Press any key to start over."

    pure $ void $ filter id eDone

exercise2 ::
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
    m Navigation
exercise2 = do
    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang `elem` [DE, EN]) elNotImplemented

    el "h1" $ text "Stage 2"
    el "h2" $ text "Syllables and chords"
    el "h3" $ text "Exercise 2"

    eDone <- case navLang of
        EN -> do
            elClass "div" "paragraph" $ do
                text
                    "We can begin with actually typing sentences now. \
                    \How about this one sentences I found in the "
                elAttr
                    "a"
                    ( "href"
                          =: "http://www.openstenoproject.org/palantype/tutorial/2016/08/21/learn-palantype.html"
                    )
                    $ text "Palantype Tutorial"
                text
                    " of the Open Steno Project? In case you have trouble \
                    \with keys that appear twice on the steno keyboard: There is \
                    \always only exactly one correct key that results in the desired \
                    \output."

            let raw = "TH CFIC P+RAUN FOCS +YUMPS OEFR TH LE^/S+I T+OC+ ^"
                lsWord =
                    [ "The",
                      "quick",
                      "brown",
                      "fox",
                      "jumps",
                      "over",
                      "the",
                      "lazy",
                      "",
                      "dog",
                      "."
                    ]

            eDone <- walkWords lsWord raw

            elClass "div" "paragraph" $ do
                text
                    "Each word is one chord, except the word \"lazy\". You will \
                    \have to strike "
                el "code" $ text "LE^"
                text " and "
                el "code" $ text "S+I"
                text
                    " separately. For this reason, the steno code for \"lazy\" \
                    \is typically denoted "
                el "code" $ text "LE^/S+I"
                text ", with a /."

            elClass "div" "paragraph" $ do
                text
                    "You wonder why the steno code looks so weird? \
                    \Some words are almost beyond recognition. \
                    \No worries, we'll get to that."

            pure eDone
        DE -> do
            elClass "div" "paragraph" $
                text
                    "We can begin with actually typing sentences now. \
                    \How about this quote from Goethe? In case you have trouble \
                    \with keys that appear twice on the steno keyboard: There is \
                    \always only exactly one correct key that results in the desired \
                    \output."

            -- TODO: punctuation
            let raw = "MID DEM WISn WÄ+GSD DEÜ SWEI/FEL N-"
                txt = "Mit dem Wissen wächst der Zwei fel ."

            eDone <- walkWords (Text.words txt) raw

            elClass "div" "paragraph" $ do
                text
                    "Each word is one chord, except the word «Zweifel». You will \
                    \have to strike "
                el "code" $ text "SWEI"
                text " and "
                el "code" $ text "FEL"
                text
                    " separately. For this reason, the steno code for «Zweifel» \
                    \is typically denoted "
                el "code" $ text "SWEI/FEL"
                text ", with a /."

            elClass "div" "paragraph" $ do
                text
                    "You wonder why the steno code looks so weird? \
                    \Some words are almost beyond recognition. \
                    \No worries, we'll get to that."

            pure eDone

    elClass "div" "paragraph" $ do
        text "Let me introduce yet another useful chord: "
        el "code" $ text $ showt $ KI.toRaw @key kiBackUp
        text
            ". It is the homerow of your right hand and deletes your last \
            \input. Now you can correct your mistakes!"

    elCongraz eDone envNavigation

    pure envNavigation

-- Ex 2.3

data StateSingletons = StateSingletons
    { _stDone :: Bool
    , _stStep :: Step
    }

data Step
    = StepPause
    | StepRun Run

data Run = Run
    { _stCounter   :: Int
    , _stMMistake  :: Maybe Mistake
    , _stWords     :: [Text]
    , _stNMistakes :: Int
    }

data Mistake
    = MistakeOne RawSteno
    | MistakeTwo RawSteno [RawSteno]

makeLenses ''StateSingletons
-- makeLenses ''StateStep
makeLenses ''Run
makePrisms ''Step

taskSingletons
    :: forall key t (m :: * -> *)
    . ( DomBuilder t m
      , MonadFix m
      , MonadHold t m
      , MonadReader (Env t key) m
      , Palantype key
      , PostBuild t m
      , Prerender t m
      )
    => Event t (Map RawSteno Text, Map Text [RawSteno])
    -> m (Event t ())
taskSingletons eMaps = do
    eChord <- asks envEChord

    eStdGen <- postRender $ do
        ePb <- getPostBuild
        performEvent $ ePb $> liftIO newStdGen

    dynMStdGen <- holdDyn Nothing $ Just <$> eStdGen
    dynMMaps   <- holdDyn Nothing $ Just <$> eMaps
    let evReady = catMaybes $ updated $
            zipDyn dynMStdGen dynMMaps <&> \(mStdGen, mMaps) -> do
                stdGen <- mStdGen
                maps   <- second (Map.take 10) <$> mMaps
                pure (stdGen, maps)

    fmap switchDyn $ widgetHold (loading $> never) $ evReady <&>
      \(stdGen, (mapStenoWord, mapWordStenos)) -> do

        let
            len = Map.size mapWordStenos
            step
                :: Chord key
                -> StateSingletons
                -> StateSingletons
            step c st = case st of
                StateSingletons _ StepPause ->
                    if Raw.fromChord c `elem` (KI.toRaw @key <$> kiChordsStart)
                        then stepStart
                        else st
                StateSingletons _ (StepRun Run{..}) ->
                    let
                        raw = Raw.fromChord c
                        word = _stWords !! _stCounter
                        isCorrect =
                            Map.findWithDefault "" raw mapStenoWord == word
                    in  if isCorrect
                            then if _stCounter == len - 1
                                     then st & stDone .~ True
                                             & stStep .~ StepPause
                                     else st & stStep
                                             . _StepRun %~ (stCounter +~ 1)
                                                         . (stMMistake .~ Nothing)
                            else case _stMMistake of
                                    -- first mistake
                                    Nothing -> st & stStep
                                                  . _StepRun
                                                  . stMMistake ?~ MistakeOne raw
                                    -- second mistake
                                    Just (MistakeOne _) ->
                                        let corrects =
                                                Map.findWithDefault [] word mapWordStenos
                                        in  st & stStep
                                               . _StepRun
                                               . stMMistake
                                               ?~ MistakeTwo raw corrects
                                    -- third mistake and so forth
                                    Just (MistakeTwo _ _) -> st
            stepStart =
                StateSingletons
                    { _stDone = False
                    , _stStep = StepRun Run
                        { _stCounter   = 0
                        , _stMMistake  = Nothing
                        , _stWords     =
                              evalRand (shuffleM $ Map.keys mapWordStenos) stdGen
                        , _stNMistakes = 0
                        }
                    }

        let stateInitial = StateSingletons
                { _stDone = False
                , _stStep = StepPause
                }
        dynSingletons <- foldDyn step stateInitial eChord

        evDone <- updated <$> holdUniqDyn (_stDone <$> dynSingletons)

        elClass "div" "taskSingletons" $ do
            dyn_ $ dynSingletons <&> \case
                StateSingletons _ StepPause -> el "div" $ do
                    text "Type "
                    elClass "span" "btnSteno blinking" $ do
                        text "Start "
                        el "code" $ text "SDAÜD"
                    text " to begin the exercise."
                StateSingletons _ (StepRun Run{..}) -> when (_stCounter < len) $
                    elClass "span" "exerciseField" $
                        elClass "div" "exerciseField" $ el "code" $ text $
                            _stWords !! _stCounter

            let evMMMistake =
                    updated dynSingletons <&> preview (stStep . _StepRun . stMMistake)
            widgetHold_ blank $ evMMMistake <&> \m -> whenJust (join m) \case
                    MistakeOne raw -> do
                        elClass "code" "red small" $ text $ showt raw
                        elClass "span" "small" $ text " try again!"
                    MistakeTwo raw corrects -> do
                        elClass "code" "red small" $ text $ showt raw
                        elClass "span" "small" $ text $
                            if length corrects == 1
                                then " try this: "
                                else " try one of these: "
                        for_ corrects $ \correct ->
                            elClass "code" "small" $ text $ showt correct

        let dynMCounter = dynSingletons <&> preview (stStep . _StepRun . stCounter)
        dyn_ $ dynMCounter <&> \mc -> whenJust mc \c ->
            elClass "div" "paragraph" $ do
                el "strong" $ text $ showt c
                text " / "
                text $ showt len

        -- widgetHold_ blank $ evDone <&> \bDone ->
        --     when bDone $ elClass "div" "small anthrazit" $
        --         text "Cleared. Press any key to start over."

        pure $ void $ filter id evDone

exercise3 ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      Palantype key,
      PostBuild t m,
      Prerender t m,
      SetRoute t (R FrontendRoute) m
    ) =>
    m Navigation
exercise3 = do
    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang == DE) elNotImplemented

    el "h1" $ text "Stage 2"
    el "h2" $ text "Syllables and chords"
    el "h3" $ text "Exercise 3"

    elClass "div" "paragraph" $
        text
            "During the following exercises, you will learn to type words, \
            \starting with the most straightfoward ones. \
            \You will be able to guess the correct chord without problem."

    elClass "div" "paragraph" $
        text
            "There are rules that will become progressively more complex \
            \in the course of this tutorial. We start simple:"

    el "h4" $ text "Rule 1: Steno key order"

    elClass "div" "paragraph" $
        text
            "Every chord consists of up to ten keys \
            \pressed at once. \
            \Within one chord, the order in which you press down keys does not \
            \matter. \
            \Instead, all keys of one chord will always be interpreted \
            \in their proper order."

    elClass "div" "paragraph" $ do
        text "For example, the steno keys "
        el "code" $ text "BUʃ"
        text
            " can only appear in exactly that order and always mean «Busch». \
            \The word «Schub» has to be typed using different keys \
            \, and indeed here it is: "
        el "code" $ text "SJUB"
        text "."

    el "h4" $ text "Rule 2: Word part structure"

    elClass "div" "paragraph" $
        text
            "One chord either makes a word or a word part. \
            \In general, a part consists of an onset, a nucleus, \
            \and a coda. The onset comprises the consonants in the beginning \
            \and can be missing. The nucleus comprises the vowels that follow \
            \and the coda finally comprises the consonants in the end."

    elClass "div" "paragraph" $
        text
            "For a word part structured that way, you will use the fingers \
            \of your left hand for the consonants of the onset and the fingers \
            \of your right hand for the consonants of the coda. \
            \For the nucleus you have your thumbs."

    elClass "div" "paragraph" $ do
        text "The example «Busch» shows, how "
        el "em" $ text "b"
        text " and "
        el "em" $ text "sch"
        text
            " have different steno keys, depending on where they appear. \
            \In the onset "
        el "em" $ text "b"
        text " is simply "
        el "code" $ text "B"
        text ", whereas in the coda, "
        el "em" $ text "b"
        text " is "
        el "code" $ text "P"
        text "."

    el "h4" $ text "Practice simple words"

    elClass "div" "paragraph" $ do
        text
            "To get started, we start with the most simple words. \
            \Every letter can be typed as it is, just make sure to use the \
            \right finger. The only specialy for now is -sch in the coda, \
            \for which you will have to use "
        el "code" $ text "ʃ"
        text "."

    ePb <- postRender $ delay 0.1 =<< getPostBuild
    eEDict <- request $ getDictDE' PatSimple 0 ePb

    widgetHold_ loading $ eEDict <&> \case
        Right _   -> blank
        Left  str -> elClass "div" "paragraph small red" $
            text $ "Could not load resource: " <> str

    eDone <- taskSingletons $ filterRight eEDict

    elCongraz eDone envNavigation
    pure envNavigation

-- Ex 2.4

exercise4 ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadFix m,
      MonadHold t m,
      MonadIO (Performable m),
      MonadReader (Env t key) m,
      Palantype key,
      PerformEvent t m,
      PostBuild t m,
      Prerender t m,
      RouteToUrl (R FrontendRoute) m,
      SetRoute t (R FrontendRoute) m,
      TriggerEvent t m
    ) =>
    m Navigation
exercise4 = do
    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang == DE) elNotImplemented

    el "h1" $ text "Stage 2"
    el "h2" $ text "Syllables and chords"
    el "h3" $ text "Exercise 4"

    elClass "div" "paragraph" $
        text
            "Introducing words that rely on two or more chords to type now. \
            \In general, the idea of any steno system is typing efficiency \
            \and the less chords you need to type a word, the better. \
            \There are, however, several reasons why especially German words \
            \do not always fit into one steno chord. In that case, \
            \the words are simply split up and you type the corresponding chord \
            \in succession to produce the word."

    elClass "div" "paragraph" $ do
        text
            "As a rule of thumb, words are split up along their ortographic \
            \syllables, e.g. we saw «Zweifel» as "
        el "code" $ text "SWEI/FEL"
        text " in "
        routeLink (stageUrl navLang $ $readLoc "stage_2-2") $ text "Exercise 2.2"
        text ". In the same exercise we saw «Wissen» as "
        el "code" $ text "WISn"
        text
            ", thus ortographic syllables are not respected, after all? \
            \Well, there are alternative spellings and you can choose \
            \to type "
        el "code" $ text "WIS/SEN"
        text ", too. Thus our next rule: "

    el "h4" $ text "Rule 3: syllables and word parts"

    elClass "div" "paragraph" $
        text
            "Words that contain more than one syllable can be typed \
            \by typing the syllables separately, one after the other."

    elClass "div" "paragraph" $ text "Type the following words as they appear!"

    ePb <- postRender $ delay 0.1 =<< getPostBuild
    evEDict <- request $ getDictDE' PatSimpleMulti 0 ePb
    evEDoc <- request $ getDocDEPattern' PatSimple 0 ePb

    evDone <- fmap switchDyn $ widgetHold (loading $> never) $
        evEDict <&> \case
            Right (mST, mTSs) -> taskWords mST mTSs
            Left str -> never <$ elClass "div" "paragraph small red"
                        (text $ "Could not load resource: dict: " <> str)

    elClass "div" "paragraph" $
        text
            "Like with a lot of rules, there are exceptions. \
            \We don't need to bother right now, the words of this exercise \
            \are not affected. Just that you now, \
            \sometimes you will have to type chords that span multiple \
            \syllables and sometimes you will need multiple chords to \
            \type a single syllable. For this reason we generally speak of \
            \word parts instead of syllables."

    elClass "div" "paragraph" $
        text
            "For completeness sake, find below the \"substitution rules\" \
            \that have been applied so far. They look trivial still, but will \
            \more complicated soon enough."

    widgetHold_ loading $
        evEDoc <&> \case
            Right doc -> elPatterns doc
            Left str ->
                elClass "div" "paragraph small red"
                    $ text
                    $ "Could not load resource: doc: " <> str

    elClass "div" "paragraph" $
        text
            "Each lowercase letter in the table is a letter of natural \
            \language. Next to it you find a steno code, denoted as \
            \uppercase letter. The entries are sorted alphabetically. \
            \Remember, though, letters generally have different steno codes \
            \depending where we are: The onset is the beginning of a word \
            \part (think of a syllable), the nucleus is one or more vowels \
            \thereafter and the coda is what comes at the end of a word part."

    elClass "div" "paragraph" $ do
        text "E.g. the letter "
        el "em" $ text "k"
        text " is only simply "
        el "code" $ text "K"
        text
            " when it appears in the coda. In practice, this won't be a \
            \problem as the steno key "
        el "code" $ text "K"
        text " exists for your right hand only, anyway."

    elCongraz evDone envNavigation
    pure envNavigation
