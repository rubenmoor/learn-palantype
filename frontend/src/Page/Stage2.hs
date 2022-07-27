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

import           Client                         ( getDictDE'
                                                , getDocDEPattern'
                                                , postRender
                                                , request
                                                )
import           Common.Route                   ( FrontendRoute(..) )
import           Control.Applicative            ((<*>),  (<$>)
                                                , Applicative(pure)
                                                )
import           Control.Category               ( Category((.), id)
                                                , (<<<)
                                                )
import           Control.Lens                   (preview, (^?),  (+~)
                                                , (?~)
                                                , (%~)
                                                , (.~)
                                                , (<&>)
                                                )
import Control.Lens.TH ( makePrisms, makeLenses )
import           Control.Monad                  ( (=<<)
                                                , unless
                                                , when
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Random           ( evalRand )
import           Control.Monad.Reader           ( MonadReader(ask)
                                                )
import           Data.Bool                      (not,  Bool(..) )
import           Data.Either                    ( Either(..) )
import           Data.Eq                        ( Eq((==)) )
import           Data.Foldable                  ( Foldable(length)
                                                , for_
                                                , traverse_
                                                )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( ($>)
                                                , void
                                                )
import           Data.Functor                   ( Functor((<$)) )
import           Data.Functor                   ( Functor(fmap) )
import           Data.Generics.Product          ( field )
import Data.Generics.Sum (_As)
import           Data.Int                       ( Int )
import           Data.List                      ( (!!)
                                                , elem
                                                , zip
                                                , head
                                                , take
                                                , cycle
                                                )
import qualified Data.Map                      as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     (fromMaybe,  Maybe(..) )
import           Data.Ord                       ( Ord((>)) )
import           Data.Semigroup                 ( (<>)
                                                , Endo
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Witherable                ( Filterable(catMaybes, filter)
                                                )
import           GHC.Num                        ( Num((+), (-)) )
import           Obelisk.Route.Frontend         (Routed, R
                                                , RouteToUrl
                                                , SetRoute(setRoute)
                                                , routeLink
                                                )
import           Page.Common                    (elBtnSound, getStatsLocalAndRemote, elBackUp
                                                , elCongraz
                                                , elNotImplemented
                                                , elPatterns
                                                , loading
                                                , taskWords
                                                )
import Page.Common.Stopwatch ( elStopwatch, mkStopwatch )
import           Palantype.Common               ( kiChordsStart
                                                , Chord(..)
                                                , Lang(..)
                                                , Palantype
                                                , fromChord
                                                , mkChord
                                                , allKeys
                                                )
import           Palantype.Common               ( kiBackUp
                                                )
import           Palantype.Common               ( RawSteno
                                                , parseStenoMaybe
                                                )
import qualified Palantype.Common.Indices      as KI
import           Palantype.DE                   ( Pattern(..) )
import           Reflex.Dom                     (constDyn, Dynamic, current, gate,  TriggerEvent
                                                , Performable
                                                , holdUniqDyn
                                                , (=:)
                                                , DomBuilder
                                                , EventName(Click)
                                                , EventWriter
                                                , HasDomEvent(domEvent)
                                                , MonadHold(holdDyn)
                                                , PerformEvent(performEvent)
                                                , PostBuild(getPostBuild)
                                                , Prerender
                                                , Reflex(Event, never, updated)
                                                , blank
                                                , delay
                                                , dyn_
                                                , el
                                                , elAttr
                                                , elClass
                                                , elClass'
                                                , elDynClass
                                                , filterRight
                                                , foldDyn
                                                , leftmost
                                                , switchDyn
                                                , text
                                                , widgetHold
                                                , widgetHold_
                                                , zipDyn
                                                )
import           Shared                         (whenJust )
import           State                          ( Env(..)
                                                , Navigation(..)
                                                , State(..)
                                                , stageUrl
                                                , updateState
                                                )
import           System.Random                  ( newStdGen )
import           System.Random.Shuffle          ( shuffleM )
import           Text.Read                      ( readMaybe )
import           TextShow                       ( TextShow(showt) )
import           Palantype.Common.TH            ( fromJust
                                                , readLoc
                                                )
import qualified Palantype.Common.RawSteno     as Raw
import           Data.Function                  ( (&) )
import Common.Model (Stats)
import Common.Stage (Stage)
import GHC.Generics (Generic)

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
                    if Raw.fromChord c `elem` (KI.toRaw @key <$> kiChordsStart)
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

        elClass "div" "taskSingletons" $ do
            dyn_ $ dynSingletons <&> \case
                StateLettersPause _ -> el "div" $ do
                    text "Type "
                    elClass "span" "btnSteno blinking" $ do
                        text "Start "
                        el "code" $ text $ showt $ KI.toRaw @key $ head
                            kiChordsStart
                    text " to begin the exercise."
                StateLettersRun SLRun {..} -> do
                    elClass "span" "exerciseField"
                        $  el "code"
                        $  text
                        $  showt
                        $  _stlLetters
                        !! _stlCounter

                    whenJust _stlMMistake \case
                            SLMistakeOne c -> do
                                elClass "code" "red small" $ text $ showt c
                                elClass "span" "small" $ text " try again!"
                            SLMistakeTwo c -> do
                                elClass "code" "red small" $ text $ showt c
                                elClass "span" "small" $ text " keep trying!"

                    elClass "hr" "visibilityHidden" blank
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
       , Routed t Stage m
       , SetRoute t (R FrontendRoute) m
       , TriggerEvent t m
       )
    => m Navigation
exercise1 = mdo
    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang `elem` [DE, EN]) elNotImplemented

    el "h1" $ text "Stage 2"
    el "h2" $ text "Syllables and chords"
    el "h3" $ text "Exercise 1"
    el "p" $ do
        text
            "You probably have noticed that on the virtual keyboard some keys \
            \are highlighted blue. These keys are called home row. \
            \The idea is that there is a resting position for your hands, where \
            \every finger is placed on one key of home row."
    el "p" $ do
        text
            "From this resting position your fingers can find any key, without \
            \any need to look down on your keyboard, so your eyes can stay on \
            \the screen all the time."
    el "p" $ do
        text
            "In case you have been looking on your fingers during the exercises, \
            \it's probably a good idea to get used to home row now. \
            \Simply repeat Stage 1 until you can do all the exercises without \
            \looking down. Just orient yourself once in the beginning!"

    let eChordBackUp =
          void $ gate (not <$> current dynDone) $
            filter (\c -> KI.fromChord c == kiBackUp) envEChord

    elABack <- el "p" $ do
        text "Type "
        elBackUp @key
        text " to "
        (e, _) <- elClass' "a" "normalLink" $ text "go back to Exercise 1.1"
        text " to practice home row."
        pure e

    let eBack = leftmost [eChordBackUp, domEvent Click elABack]

    setRoute $ eBack $> stageUrl navLang ($readLoc "stage_1-1")
    updateState
        $  eBack
        $> [ field @"stApp" . field @"stProgress"
                 %~ Map.update (\_ -> Just $ $readLoc "stage_1-1") navLang
           ]

    el "p" $ do
        text
            "Also, just like looking down on your fingers will slow you down, \
            \you can't actually use the virtual keyboard if you want to get up \
            \to speed. I recommend, turning it off. The following exercise \
            \is an important prerequisite to actually learn the chords."
    el "p" $ do
        text
            "Maybe, you want to come up with a mnemonic phrase that helps \
            \remembering the keyboard layout, e.g."
    el "p" $ do
        el "div" $ do
            el "strong" $ text "V"
            text "er"
            el "strong" $ text "D"
            text "or"
            el "strong" $ text "b"
            text "ene"
        el "div" $ do
            el "strong" $ text "Sch"
            text "warze "
            el "strong" $ text "S"
            text "tern"
            el "strong" $ text "F"
            text "rucht"
        el "div" $ do
            el "strong" $ text "G"
            text "e"
            el "strong" $ text "N"
            text "ieß"
            el "strong" $ text "B"
            text "ar"
        el "div" $ do
            text "mit "
            el "strong" $ text "M"
            text "ilch "
            el "strong" $ text "+"
            el "strong" $ text " L"
            text "ebertran"
    el "p" $ do
        el "div" $ do
            el "strong" $ text "Ä"
            text "tna-"
            el "strong" $ text "E"
            text "ruption "
            el "strong" $ text "A"
            text "ußergewöhnlich "
            el "strong" $ text "Lang"
    el "p" $ do
        el "div" $ do
            el "strong" $ text "U"
            text "rgewalt "
            el "strong" $ text "I"
            text "mmens "
            el "strong" $ text "O"
            text "hne "
            el "strong" $ text "Ü"
            text "berlebende "

    el "p" $ do
        text "You can surely come up with a better one, yourself!"

    el "p" $ text "This one is for the homerow:"

    el "p" $ do
      el "strong" $ text "D"
      text "ussel "
      el "strong" $ text "S"
      text "ind "
      el "strong" $ text "N"
      text "iemand "
      el "strong" $ text "+"
      el "strong" $ text " A"
      text "lle, "
      el "strong" $ text "I"
      text "mmer "
      el "strong" $ text "+"
      el "strong" $ text " N"
      text "ie, "
      el "strong" $ text "S"
      text "ie "
      el "strong" $ text "D"
      text "ussel!"

    dynStats <- getStatsLocalAndRemote evDone
    evDone <- taskLetters dynStats (gate (not <$> current dynDone) envEChord)
    dynDone <- elCongraz (Just <$> evDone) dynStats envNavigation
    pure envNavigation

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
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       )
    => Event t (Chord key)
    -> [Text]
    -> RawSteno
    -> m (Event t ())
walkWords evChord words raw = do
    Env {..} <- ask
    let Navigation {..} = envNavigation

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

    el "blockquote" $ el "table" $ do
        el "tr" $ traverse_ (elAttr "td" ("colspan" =: "2") . text) words
        el "tr" $ do
            for_ (zip [0 :: Int ..] chords) $ \(i, c) -> do
                let dynCls = dynWalk <&> \WalkState {..} -> case wsMMistake of
                        Just j  -> if i == j then "bgRed" else ""
                        Nothing -> if wsCounter > i then "bgGreen" else ""
                when (i > 0) $ el "td" $ elClass "span" "darkgray" $ text "•"
                el "td" $ elDynClass "span" dynCls $ el "code" $ text $ showt c

            el "td" $ do
                let eMistake = wsMMistake <$> updated dynWalk
                widgetHold_ blank $ eMistake <&> \case
                    Just _ -> elClass "code" "blinking" $ do
                        text " "
                        elBackUp @key
                    Nothing -> blank

    dynDone <- holdDyn False evDone
    dyn_ $ dynDone <&> \bDone ->
        when bDone $ elClass "div" "small anthrazit" $ text
            "Cleared. Press any key to start over."

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
       , Routed t Stage m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise2 = mdo
    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang `elem` [DE, EN]) elNotImplemented

    el "h1" $ text "Stage 2"
    el "h2" $ text "Syllables and chords"
    el "h3" $ text "Exercise 2"

    evDone <- case navLang of
        EN -> do
            el "p" $ do
                text
                    "We can begin with actually typing sentences now. \
                    \How about this one sentences I found in the "
                elAttr
                        "a"
                        ("href"
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
                    [ "The"
                    , "quick"
                    , "brown"
                    , "fox"
                    , "jumps"
                    , "over"
                    , "the"
                    , "lazy"
                    , ""
                    , "dog"
                    , "."
                    ]

            evDone' <- walkWords (gate (not <$> current dynDone) envEChord) lsWord raw

            el "p" $ do
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

            el "p" $ do
                text
                    "You wonder why the steno code looks so weird? \
                    \Some words are almost beyond recognition. \
                    \No worries, we'll get to that."

            pure evDone'
        DE -> do
            el "p"
                $ text
                      "We can begin with actually typing sentences now. \
                    \How about this quote from Goethe? In case you have trouble \
                    \with keys that appear twice on the steno keyboard: There is \
                    \always only exactly one correct key that results in the desired \
                    \output."

            -- TODO: punctuation
            let raw = "MID DEM F+ISn F+Ä+Gʃs DEÜ ʃG+EI/FEL +-"
                txt = "Mit dem Wissen wächst der Zwei fel ."

            evDone' <- walkWords (gate (not <$> current dynDone) envEChord) (Text.words txt) raw

            el "p" $ do
                text
                    "Each word is one chord, except the word «Zweifel». You will \
                    \have to strike "
                el "code" $ text "ʃG+EI"
                text " and "
                el "code" $ text "FEL"
                text
                    " separately. For this reason, the steno code for «Zweifel» \
                    \is typically denoted "
                el "code" $ text "ʃG+EI/FEL"
                text ", with a /."

            el "p" $ do
                text
                    "You wonder why the steno code looks so weird? \
                    \Some words are almost beyond recognition. \
                    \No worries, we'll get to that."

            pure evDone'

    el "p" $ do
        text "Let me introduce yet another useful chord: "
        el "code" $ text $ showt $ KI.toRaw @key kiBackUp
        text
            ". It is the homerow of your right hand and deletes your last \
            \input. Now you can correct your mistakes!"

    dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

    pure envNavigation

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
    -> Event t (Map RawSteno Text, Map Text [RawSteno])
    -> m (Event t Stats)
taskSingletons dynStats evChord eMaps = do

    Env{..} <- ask
    eStdGen <- postRender $ do
        ePb <- getPostBuild
        performEvent $ ePb $> liftIO newStdGen

    dynMStdGen <- holdDyn Nothing $ Just <$> eStdGen
    dynMMaps   <- holdDyn Nothing $ Just <$> eMaps
    let evReady =
            catMaybes
                $   updated
                $   zipDyn dynMStdGen dynMMaps
                <&> \(mStdGen, mMaps) -> (,) <$> mStdGen <*> mMaps

    fmap switchDyn
        $   widgetHold (loading $> never)
        $   evReady
        <&> \(stdGen, (mapStenoWord, mapWordStenos)) -> do

                let
                    len = Map.size mapWordStenos

                    step :: Chord key -> StateSingletons -> StateSingletons
                    step c st = case st of
                        StatePause _ ->
                            if Raw.fromChord c
                                `elem` (KI.toRaw @key <$> kiChordsStart)
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
                                            st
                                            &  _StateRun
                                            %~ (stCounter +~ 1)
                                            .  (stMMistake .~ Nothing)
                                    else case _stMMistake of
                                            -- first mistake
                                        Nothing -> st &  _StateRun
                                          %~ ( stMMistake ?~ MistakeOne raw )
                                          .  ( stNMistakes +~ 1)
                                        -- second mistake
                                        Just (MistakeOne _) ->
                                            let
                                                corrects = Map.findWithDefault
                                                    []
                                                    word
                                                    mapWordStenos
                                            in  st
                                                    &  _StateRun
                                                    .  stMMistake
                                                    ?~ MistakeTwo raw corrects
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

                elClass "div" "taskSingletons" $ do

                    evTrigger <- void . updated <$> holdUniqDyn
                      ( dynSingletons <&> fromMaybe 0
                          . preview (_As @"StateRun" . field @"_stCounter")
                      )
                    elBtnSound evTrigger

                    dyn_ $ dynSingletons <&> \case
                        StatePause _ -> el "div" $ do
                            text "Type "
                            elClass "span" "btnSteno blinking" $ do
                                text "Start "
                                el "code" $ text $ showt $ KI.toRaw @key $ head
                                    kiChordsStart
                            text " to begin the exercise."
                        StateRun Run {..} -> do
                            elClass "span" "exerciseField"
                                $  el "code"
                                $  text
                                $  _stWords
                                !! _stCounter

                            whenJust
                                _stMMistake
                                \case
                                    MistakeOne raw -> do
                                        elClass "code" "red small"
                                            $ text
                                            $ showt raw
                                        elClass "span" "small"
                                            $ text " try again!"
                                    MistakeTwo raw corrects -> do
                                        elClass "code" "red small"
                                            $ text
                                            $ showt raw
                                        elClass "span" "small"
                                            $ text
                                            $ if length corrects == 1
                                                  then " try this: "
                                                  else " try one of these: "
                                        for_ corrects
                                            $ \correct ->
                                                  elClass "code" "small"
                                                      $ text
                                                      $ showt correct

                            elClass "hr" "visibilityHidden" blank
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
       , Routed t Stage m
       , SetRoute t (R FrontendRoute) m
       , TriggerEvent t m
       )
    => m Navigation
exercise3 = mdo
    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang == DE) elNotImplemented

    el "h1" $ text "Stage 2"
    el "h2" $ text "Syllables and chords"
    el "h3" $ text "Exercise 3"

    el "p"
        $ text
              "During the following exercises, you will learn to type words, \
            \starting with the most straightfoward ones. \
            \You will be able to guess the correct chord without problem."

    el "p"
        $ text
              "There are rules that will become progressively more complex \
            \in the course of this tutorial. We start simple:"

    el "h4" $ text "Rule 1: Steno key order"

    el "p"
        $ text
              "Every chord consists of up to ten keys \
            \pressed at once. \
            \Within one chord, the order in which you press down keys does not \
            \matter. \
            \Instead, all keys of one chord will always be interpreted \
            \in their proper order."

    el "p" $ do
        text "For example, the steno keys "
        el "code" $ text "BUʃ"
        text
            " can only appear in exactly that order and always mean «Busch». \
            \The word «Schub» has to be typed using different keys \
            \, and indeed here it is: "
        el "code" $ text "ʃUB"
        text "."
    el "p" $ do
      text "In conclusion, it "
      el "em" $ text "does"
      text " matter which key (or which hand) you use: "
      el "code" $ text "ʃ-"
      text " isn't the same as "
      el "code" $ text "-ʃ"
      text ". But it "
      el "em" $ text "does not"
      text " matter in which order you actually put down your fingers. \
           \Ideally you press all the keys of one chord at the same time."

    el "h4" $ text "Rule 2: Word part structure"

    el "p"
        $ text
              "One chord either makes a word or a word part. \
            \In general, a part consists of an onset, a nucleus, \
            \and a coda. The onset comprises the consonants in the beginning \
            \and can be missing. The nucleus comprises the vowels that follow \
            \and the coda finally comprises the consonants in the end."

    el "p"
        $ text
              "For a word part structured that way, you will use the fingers \
            \of your left hand for the consonants of the onset and the fingers \
            \of your right hand for the consonants of the coda. \
            \For the nucleus you have your thumbs."

    el "h4" $ text "Practice simple words"

    el "p" $ do
        text
            "To get started, we start with the most simple words. \
            \Every letter can be typed as it is, just make sure to use the \
            \correct finger. The only specialty for now is \"sch\", \
            \for which you will have to use "
        el "code" $ text "ʃ"
        text ". And remember: the small keys "
        el "code" $ text "v"
        text ", "
        el "code" $ text "b"
        text ", "
        el "code" $ text "s"
        text ", and "
        el "code" $ text "n"
        text " are special keys. Don't use them yet."

    ePb    <- postRender $ delay 0.1 =<< getPostBuild
    eEDict <- request $ getDictDE' PatSimple 0 ePb

    widgetHold_ loading $ eEDict <&> \case
        Right _ -> blank
        Left str ->
            elClass "div" "paragraph small red"
                $  text
                $  "Could not load resource: "
                <> str

    dynStats <- getStatsLocalAndRemote evDone
    evDone <- taskSingletons dynStats (gate (not <$> current dynDone) envEChord) $ filterRight eEDict

    dynDone <- elCongraz (Just <$> evDone) dynStats envNavigation
    pure envNavigation

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
       , Routed t Stage m
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       , TriggerEvent t m
       )
    => m Navigation
exercise4 = mdo
    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang == DE) elNotImplemented

    el "h1" $ text "Stage 2"
    el "h2" $ text "Syllables and chords"
    el "h3" $ text "Exercise 4"

    el "p"
        $ text
              "Introducing words that rely on two or more chords to type now. \
            \In general, the idea of any steno system is typing efficiency \
            \and the less chords you need to type a word, the better. \
            \There are, however, several reasons why especially German words \
            \do not always fit into one steno chord. In that case, \
            \the words are simply split up and you type the corresponding chord \
            \in succession to produce the word."

    el "p" $ do
        text
            "As a rule of thumb, words are split up along their ortographic \
            \syllables, e.g. we saw «Zweifel» as "
        el "code" $ text "SWEI/FEL"
        text " in "
        routeLink (stageUrl navLang $ $readLoc "stage_2-2")
            $ text "Exercise 2.2"
        text ". In the same exercise we saw «Wissen» as "
        el "code" $ text "WISn"
        text
            ", thus ortographic syllables are not respected, after all? \
            \Well, there are alternative spellings and you can choose \
            \to type "
        el "code" $ text "WIS/SEN"
        text ", too. Thus our next rule: "

    el "h4" $ text "Rule 3: syllables and word parts"

    el "p"
        $ text
              "Words that contain more than one syllable can be typed \
            \by typing the syllables separately, one after the other."

    ePb     <- postRender $ delay 0.1 =<< getPostBuild
    evEDict <- request $ getDictDE' PatSimpleMulti 0 ePb
    evEDoc  <- request $ getDocDEPattern' PatSimple 0 ePb

    dynStats <- getStatsLocalAndRemote evDone
    evDone <- fmap switchDyn $ widgetHold (loading $> never) $ evEDict <&> \case
        Right (mST, mTSs) -> taskWords dynStats (gate (not <$> current dynDone) envEChord) mST mTSs
        Left  str         -> never <$ elClass
            "div"
            "paragraph small red"
            (text $ "Could not load resource: dict: " <> str)

    el "h3" $ text "Simple substitution rules"

    el "p"
        $ text
              "Like with a lot of rules, there are exceptions. \
            \We don't need to bother right now, the words of this exercise \
            \are not affected. Just that you now, \
            \sometimes you will have to type chords that span multiple \
            \syllables and sometimes you will need multiple chords to \
            \type a single syllable. For this reason we generally speak of \
            \word parts instead of syllables."

    el "p"
        $ text
              "For completeness sake, find below the \"substitution rules\" \
            \that have been applied so far. They look trivial still, but will \
            \more complicated soon enough."

    widgetHold_ loading $ evEDoc <&> \case
        Right doc -> elPatterns doc
        Left str ->
            elClass "div" "paragraph small red"
                $  text
                $  "Could not load resource: doc: "
                <> str

    el "p"
        $ text
              "Each lowercase letter in the table is a letter of natural \
            \language. Next to it you find a steno code, denoted as \
            \uppercase letter. The entries are sorted alphabetically."

    el "p" $ do
        let styleHuge = "style" =: "font-size: 48pt"
        elAttr "span" ("class" =: "bgPink" <> styleHuge) $ text "st"
        elAttr "span" ("class" =: "bgLightgreen" <> styleHuge) $ text "a"
        elAttr "span" ("class" =: "bgLightblue" <> styleHuge) $ text "rk"

    dynDone <- elCongraz (Just <$> evDone) dynStats envNavigation
    pure envNavigation
