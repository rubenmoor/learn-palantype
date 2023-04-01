{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
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

import           CMS                            ( elCMS, elCMSContent )
import           Client                         ( postRender )
import           Common.Model                   ( Stats )
import           Common.Route                   ( FrontendRoute(..) )
import           Control.Applicative            ( (<$>)
                                                , Applicative(pure)
                                                )
import           Control.Category               ( Category((.), id) )
import           Control.Lens                   ( (%~)
                                                , (+~)
                                                , (.~)
                                                , (<&>)
                                                , (?~)
                                                , at
                                                , preview
                                                )
import           Control.Lens.TH                ( makeLenses
                                                , makePrisms
                                                )
import           Control.Monad                  ( unless
                                                , when
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Random           ( evalRand )
import           Control.Monad.Reader           ( MonadReader(ask) )
import           Data.Bool                      ( Bool(..)
                                                , not
                                                )
import           Data.Either                    ( Either(..) )
import           Data.Eq                        ( Eq((==)) )
import           Data.Foldable                  ( Foldable(length)
                                                , for_
                                                , traverse_
                                                )
import           Data.Function                  ( ($)
                                                , (&)
                                                )
import           Data.Functor                   ( ($>)
                                                , Functor(fmap)
                                                , void
                                                )
import           Data.Generics.Product          ( field )
import           Data.Generics.Sum              ( _As )
import           Data.Int                       ( Int )
import           Data.List                      ( (!!)
                                                , cycle
                                                , elem
                                                , take
                                                , zip
                                                )
import qualified Data.Map                      as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( Maybe(..)
                                                , fromMaybe
                                                , isNothing
                                                )
import           Data.Ord                       ( Ord((>)) )
import           Data.Semigroup                 ( (<>)
                                                , Endo
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Tuple                     ( fst
                                                , snd
                                                )
import           GHC.Generics                   ( Generic )
import           GHC.Num                        ( Num((+), (-)) )
import           Obelisk.Route.Frontend         ( R
                                                , SetRoute(setRoute)
                                                )
import           Page.Common                    ( elBtnSound
                                                , elCongraz
                                                , elNotImplemented
                                                , elPatterns
                                                , getStatsLocalAndRemote
                                                , loading
                                                , taskWords, chordStart
                                                )
import           Page.Common.Stopwatch          ( elStopwatch
                                                , mkStopwatch
                                                )
import           Palantype.Common               ( Chord(..)
                                                , Palantype
                                                , RawSteno
                                                , StageSpecialGeneric(..)
                                                , SystemLang(..)
                                                , allKeys
                                                , findStage
                                                , fromChord
                                                , kiBackUp
                                                , mkChord
                                                , parseStenoMaybe
                                                , patternDoc
                                                )
import qualified Palantype.Common.Indices      as KI
import qualified Palantype.Common.RawSteno     as Raw
import           Palantype.Common.TH            ( fromJust )
import           Palantype.DE                   ( Pattern(..) )
import           PloverDict                     ( getMapsForExercise )
import           Reflex.Dom                     ( (=:)
                                                , DomBuilder
                                                , Dynamic
                                                , EventWriter
                                                , MonadHold(holdDyn)
                                                , PerformEvent(performEvent)
                                                , Performable
                                                , PostBuild(getPostBuild)
                                                , Prerender
                                                , Reflex(Event, never, updated)
                                                , TriggerEvent
                                                , blank
                                                , constDyn
                                                , current
                                                , dyn_
                                                , el
                                                , elAttr
                                                , elClass
                                                , elDynClass
                                                , foldDyn
                                                , gate
                                                , holdUniqDyn
                                                , switchDyn
                                                , text
                                                , widgetHold
                                                , widgetHold_, splitE
                                                )
import           Shared                         ( whenJust )
import           State                          ( Env(..)
                                                , Navigation(..)
                                                , State(..)
                                                , stageUrl
                                                , updateState
                                                )
import           System.Random                  ( newStdGen )
import           System.Random.Shuffle          ( shuffleM )
import           TextShow                       ( TextShow(showt) )
import           Witherable                     ( Filterable
                                                    ( catMaybes
                                                    , filter
                                                    , mapMaybe
                                                    )
                                                )
import Data.Bifunctor (Bifunctor(first))

-- Ex. 2.1

data StateLetters key
    = StateLettersPause Int
    | StateLettersRun (SLRun key)

data SLRun key = SLRun
    { _stlCounter   :: Int
    , _stlMMistake  :: Maybe (SLMistake key)
    , _stlLetters   :: [key]
    , _stlNMistakes :: Int
    }

data SLMistake key
    = SLMistakeOne (Chord key)
    | SLMistakeTwo (Chord key)

makeLenses ''SLRun
makePrisms ''StateLetters

taskLetters
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
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
    => Dynamic t [(Bool, (Maybe Text, Stats))]
    -> Event t (Chord key)
    -> m (Event t Stats)
taskLetters dynStats evChord = do

    eStdGen <- postRender $ do
        ePb <- getPostBuild
        performEvent $ ePb $> liftIO newStdGen

    dynMStdGen <- holdDyn Nothing $ Just <$> eStdGen
    let evReady = catMaybes $ updated dynMStdGen

    fmap switchDyn $ widgetHold (loading $> never) $ evReady <&> \stdGen -> do

        let len = 100
            step :: Chord key -> StateLetters key -> StateLetters key
            step c st = case st of
                StateLettersPause _ ->
                    if c == chordStart
                        then stepStart
                        else st
                StateLettersRun SLRun {..} ->
                    let
                        -- raw = Raw.fromChord c
                        --  = _stlLetters !! _stlCounter
                        isCorrect = c == mkChord [_stlLetters !! _stlCounter]
                    in
                        if isCorrect
                            then if _stlCounter == len - 1
                                then StateLettersPause _stlNMistakes
                                else st &  _StateLettersRun
                                    %~ (stlCounter +~ 1)
                                    .  (stlMMistake .~ Nothing)
                            else case _stlMMistake of
                                    -- first mistake
                                Nothing ->
                                    st &  _StateLettersRun
                                        %~ ( stlMMistake ?~ SLMistakeOne c )
                                        .  ( stlNMistakes +~ 1)
                                -- second mistake
                                Just (SLMistakeOne _) ->
                                    st &  _StateLettersRun . stlMMistake ?~ SLMistakeTwo c
                                -- third mistake and so forth
                                Just (SLMistakeTwo _) ->
                                    st &  _StateLettersRun .  stlMMistake ?~ SLMistakeTwo c
            stepStart = StateLettersRun SLRun
                { _stlCounter   = 0
                , _stlMMistake  = Nothing
                , _stlLetters   = evalRand
                                      (shuffleM $ take len $ cycle allKeys)
                                      stdGen
                , _stlNMistakes = 0
                }

        let stateInitial = StateLettersPause 0
        dynSingletons <- foldDyn step stateInitial evChord
        evStartStop   <- fmap updated $ holdUniqDyn $ dynSingletons <&> \case
            StateLettersPause nMistakes -> nMistakes
            StateLettersRun   _         -> -1
        dynStopwatch <- mkStopwatch evStartStop

        elClass "div" "mt-8 text-lg" do
            dyn_ $ dynSingletons <&> \case
                StateLettersPause _ -> el "div" $ do
                    text "Type "
                    elClass "span" "steno-action" $ do
                        text "Start "
                        el "code" $ text $ showt $ chordStart @key
                    text " to begin the exercise."
                StateLettersRun SLRun {..} -> do
                    elClass "span" "bg-zinc-200 rounded w-fit p-1"
                        $  el "code"
                        $  text
                        $  showt
                        $  _stlLetters
                        !! _stlCounter

                    whenJust _stlMMistake $ elClass "span" "ml-8 text-sm" . \case
                            SLMistakeOne c -> do
                                elClass "code" "text-red-500" $ text $ showt c
                                text " try again!"
                            SLMistakeTwo c -> do
                                elClass "code" "text-red-500" $ text $ showt c
                                text " keep trying!"

                    el "br" blank
                    el "br" blank
                    el "strong" $ text $ showt _stlCounter
                    text $ " / " <> showt len

            elStopwatch dynStats dynStopwatch len

exercise1
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadIO (Performable m)
       , MonadReader (Env t key) m
       , Palantype key
       , PerformEvent t m
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       , TriggerEvent t m
       )
    => m ()
exercise1 = mdo
    Env {..} <- ask

    evContent <- elCMS 1 <&> mapMaybe \case
      [p1] -> Just p1
      _    -> Nothing

    elCMSContent evContent

    let Navigation {..} = envNavigation
    unless (navSystemLang `elem` [SystemDE, SystemEN]) elNotImplemented

    let eChordBackUp =
          void $ gate (not <$> current dynDone) $
            filter (\c -> KI.fromChord c == kiBackUp) envEChord

        (stage1_1, _, _) = $fromJust $ findStage @key (StageSpecial "Type the letters")

    setRoute $ eChordBackUp $> stageUrl @key 1 -- Stage 1.1
    updateState $ eChordBackUp $>
      [ field @"stApp" . field @"stProgress" . at navSystemLang ?~ stage1_1
      ]


    dynStatsAll <- getStatsLocalAndRemote evDone
    let dynStatsPersonal = fmap snd . filter (isNothing . fst) . fmap snd <$> dynStatsAll
    evDone <- taskLetters dynStatsAll (gate (not <$> current dynDone) envEChord)
    dynDone <- elCongraz (Just <$> evDone) dynStatsPersonal envNavigation

    pure ()


-- Ex 2.2

data WalkState = WalkState
    { wsCounter :: Int,
      wsMMistake :: Maybe Int,
      wsDone :: Maybe Bool
    }

walkWords
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , MonadFix m
       , MonadHold t m
       , Palantype key
       , PostBuild t m
       )
    => Event t (Chord key)
    -> [Text]
    -> RawSteno
    -> m (Event t ())
walkWords evChord words raw = do

    let chords = $fromJust $ parseStenoMaybe raw
        len    = length chords
        step :: Chord key -> WalkState -> WalkState
        step chord ws@WalkState {..} = case (wsMMistake, wsDone) of
        -- reset after done
            (_, Just True) -> ws { wsDone = Just False, wsCounter = 0 }
            -- undo stroke
            _ | fromChord chord == KI.toRaw @key kiBackUp ->
                ws { wsMMistake = Nothing }
            -- halt while mistake
            (Just _, _) -> ws
            -- correct
            _ | chords !! wsCounter == chord ->
                let done = if wsCounter == len - 1
                        then Just True -- done
                        else Nothing
                in  ws { wsDone = done, wsCounter = wsCounter + 1 }
            -- mistake
            _ -> ws { wsDone = Nothing, wsMMistake = Just wsCounter }
        stepInitial = WalkState { wsCounter  = 0
                                , wsMMistake = Nothing
                                , wsDone     = Nothing
                                }

    dynWalk <- foldDyn step stepInitial evChord
    let evDone = catMaybes $ wsDone <$> updated dynWalk

    elClass "table" "my-4" do
        el "tr" $ traverse_ (elAttr "td" ("colspan" =: "2") . text) words
        el "tr" do
            for_ (zip [0 :: Int ..] chords) $ \(i, c) -> do
                let dynCls = dynWalk <&> \WalkState {..} -> case wsMMistake of
                        Just j  -> if i == j then "p-1 rounded bg-red-500" else "p-1"
                        Nothing -> if wsCounter > i then "p-1 rounded bg-green-500" else "p-1"
                when (i > 0) $ el "td" $ elClass "span" "text-grayishblue-900" $ text "•"
                el "td" $ elDynClass "span" dynCls $ el "code" $ text $ showt c

            el "td" $ do
                let eMistake = wsMMistake <$> updated dynWalk
                widgetHold_ blank $ eMistake <&> \case
                    Just _ -> el "code" $ do
                        text " "
                        elClass "span" "text-sm steno-action" $
                          text $ "↤ " <> showt (KI.toRaw @key kiBackUp) -- U+21A4
                    Nothing -> blank

    dynDone <- holdDyn False evDone
    dyn_ $ dynDone <&> \bDone -> when bDone do
      elClass "div" "text-sm text-grayishblue-900" $ text
            "Cleared. Press any key to start over."
      el "br" blank

    pure $ void $ filter id evDone

exercise2
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       , TriggerEvent t m
       , PerformEvent t m
       , MonadIO (Performable m)
       )
    => m ()
exercise2 = mdo
    Env {..} <- ask
    let Navigation{..} = envNavigation

    (evPart1, evPart2) <- elCMS 2 <&> splitE .mapMaybe \case
        [p1, p2] -> Just (p1, p2)
        _        -> Nothing

    elCMSContent evPart1

    evDone <- case navSystemLang of
      SystemEN ->
        let raw = "TH CFIC P+RAUN FOCS +YUMPS OEFR TH LE^/S+I T+OC+ ^"
            txt = "The quick brown fox jumps over the la zy dog ."

        in  walkWords (gate (not <$> current dynDone) envEChord) (Text.words txt) raw

      SystemDE ->
          let raw = "MID DEM F+ISn F+Ä+GSD DEÜ ʃG+EI/FEL +-"
              txt = "Mit dem Wissen wächst der Zwei fel ."

          in  walkWords (gate (not <$> current dynDone) envEChord) (Text.words txt) raw

    dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

    elCMSContent evPart2

-- Ex 2.3

data StateSingletons
    = StatePause Int
    | StateRun Run
    deriving (Generic)

data Run = Run
    { _stCounter   :: Int
    , _stMMistake  :: Maybe Mistake
    , _stWords     :: [Text]
    , _stNMistakes :: Int
    } deriving (Generic)

data Mistake
    = MistakeOne RawSteno
    | MistakeTwo RawSteno [RawSteno]

makeLenses ''Run
makePrisms ''StateSingletons

taskSingletons
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
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
    => Dynamic t [(Bool, (Maybe Text, Stats))]
    -> Event t (Chord key)
    -> Map RawSteno Text
    -> Map Text [RawSteno]
    -> m (Event t Stats)
taskSingletons dynStats evChord mapStenoWord mapWordStenos = do

    eStdGen <- postRender $ do
        ePb <- getPostBuild
        performEvent $ ePb $> liftIO newStdGen

    dynMStdGen <- holdDyn Nothing $ Just <$> eStdGen
    let evReady = catMaybes $ updated dynMStdGen

    fmap switchDyn $ widgetHold (loading $> never) $ evReady <&> \stdGen -> do
        let
            len = Map.size mapWordStenos

            step :: Chord key -> StateSingletons -> StateSingletons
            step c st = case st of
                StatePause _ ->
                    if c == chordStart
                    then
                        stepStart
                    else
                        st
                StateRun Run {..} ->
                    let
                        raw  = Raw.fromChord c
                        word = _stWords !! _stCounter
                        isCorrect = Map.findWithDefault "" raw mapStenoWord == word
                    in
                        if isCorrect
                            then if _stCounter == len - 1
                                then StatePause _stNMistakes
                                else
                                    st &  _StateRun %~ (stCounter +~ 1) . (stMMistake .~ Nothing)
                            else case _stMMistake of
                                    -- first mistake
                                Nothing -> st & _StateRun %~ ( stMMistake ?~ MistakeOne raw ) .  ( stNMistakes +~ 1)
                                -- second mistake
                                Just (MistakeOne _) ->
                                    let corrects = Map.findWithDefault [] word mapWordStenos
                                    in  st &  _StateRun .  stMMistake ?~ MistakeTwo raw corrects
                                -- third mistake and so forth
                                Just (MistakeTwo _ _) -> st
            stepStart = StateRun Run
                { _stCounter   = 0
                , _stMMistake  = Nothing
                , _stWords     = evalRand
                                      (shuffleM $ Map.keys mapWordStenos)
                                      stdGen
                , _stNMistakes = 0
                }

        let stateInitial = StatePause 0
        dynSingletons <- foldDyn step stateInitial evChord
        evStartStop   <-
            fmap updated $ holdUniqDyn $ dynSingletons <&> \case
                StatePause nMistakes -> nMistakes
                StateRun   _         -> -1
        dynStopwatch <- mkStopwatch evStartStop

        elClass "div" "mt-8 text-lg" $ do

            evTrigger <- void . updated <$> holdUniqDyn
              ( dynSingletons <&> fromMaybe 0
                  . preview (_As @"StateRun" . field @"_stCounter")
              )
            elBtnSound evTrigger

            dyn_ $ dynSingletons <&> \case
                StatePause _ -> el "div" $ do
                    text "Type "
                    elClass "span" "steno-action" $ do
                        text "Start "
                        el "code" $ text $ showt $ chordStart @key
                    text " to begin the exercise."
                StateRun Run {..} -> do
                    elClass "span" "bg-zinc-200 rounded w-fit p-1"
                        $  el "code"
                        $  text
                        $  _stWords
                        !! _stCounter

                    whenJust _stMMistake $ elClass "span" "ml-i text-sm" . \case
                        MistakeOne raw -> do
                            elClass "code" "text-red-500" $ text $ showt raw
                            text " try again!"
                        MistakeTwo raw corrects -> do
                            elClass "code" "text-red-500" $ text $ showt raw
                            text $ if length corrects == 1
                                      then " try this: "
                                      else " try one of these: "
                            for_ corrects $ \correct ->
                                el "code" $ text $ showt correct

                    el "br" blank
                    el "br" blank
                    el "strong" $ text $ showt _stCounter
                    text $ " / " <> showt len

            elStopwatch dynStats dynStopwatch len

exercise3
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadIO (Performable m)
       , MonadReader (Env t key) m
       , Palantype key
       , PerformEvent t m
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       , TriggerEvent t m
       )
    => m ()
exercise3 = mdo
    Env {..} <- ask

    evContent <- elCMS 1 <&> mapMaybe \case
      [p] -> Just p
      _   -> Nothing

    elCMSContent evContent

    dynStatsAll <- getStatsLocalAndRemote evDone
    let dynStatsPersonal = fmap snd . filter (isNothing . fst) . fmap snd <$> dynStatsAll

    evDone <- case getMapsForExercise PatSimple 0 of
        Left str -> do
            elClass "p" "text-sm text-red-500" $ text $ "Couldn't load exercise: " <> str
            pure never
        Right (mSW, mWSs) ->
            taskSingletons dynStatsAll (gate (not <$> current dynDone) envEChord) mSW mWSs

    dynDone <- elCongraz (Just <$> evDone) dynStatsPersonal envNavigation
    blank

-- Ex 2.4

exercise4
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadIO (Performable m)
       , MonadReader (Env t key) m
       , Palantype key
       , PerformEvent t m
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       , TriggerEvent t m
       )
    => m ()
exercise4 = mdo
    Env {..} <- ask

    ((evPart1, evPart2), evPart3) <- elCMS 3 <&> first splitE . splitE . mapMaybe \case
      [p1, p2, p3] -> Just ((p1, p2), p3)
      _        -> Nothing

    elCMSContent evPart1

    dynStatsAll <- getStatsLocalAndRemote evDone
    evDone      <- case getMapsForExercise PatSimpleMulti 0 of
        Left str -> do
            elClass "p" "text-sm text-red-500" $ text $ "Couldn't load exercise: " <> str
            pure never
        Right (mSW, mWSs) -> taskWords
            dynStatsAll
            (gate (not <$> current dynDone) envEChord)
            mSW
            mWSs

    let dynStatsPersonal = fmap snd . filter (isNothing . fst) . fmap snd <$> dynStatsAll
    dynDone <- elCongraz (Just <$> evDone) dynStatsPersonal envNavigation

    elCMSContent evPart2

    elPatterns
        $ Map.toList
        $ Map.findWithDefault Map.empty 0
        $ Map.findWithDefault Map.empty PatSimple patternDoc

    elCMSContent evPart3

    el "p" $ do
        let styleHuge = "style" =: "font-size: 48pt"
        elAttr "span" ("class" =: "text-rose-400" <> styleHuge) $ text "st"
        elAttr "span" ("class" =: "text-green-400" <> styleHuge) $ text "a"
        elAttr "span" ("class" =: "text-blue-400" <> styleHuge) $ text "rk"
