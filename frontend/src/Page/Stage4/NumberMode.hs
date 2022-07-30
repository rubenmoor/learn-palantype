{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Page.Stage4.NumberMode
    ( numberMode
    )
where

import           Data.Generics.Sum              ( _As )
import           Data.Generics.Product          ( field )
import           Page.Common.Stopwatch          ( elStopwatch
                                                , mkStopwatch
                                                )
import           Client                         ( getDictDENumbers
                                                , postRender
                                                , request
                                                )
import           Common.Route                   ( FrontendRoute )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Monad                  ( (=<<) )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.Reader.Class     ( MonadReader
                                                , ask
                                                )
import           Data.Either                    ( Either(..) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( ($>)
                                                , (<&>)
                                                )
import           Data.Functor                   ( (<$>) )
import           Data.Int                       ( Int )
import           Data.List                      ( (!!)
                                                , repeat
                                                )
import           Data.Maybe                     (isNothing,  maybe
                                                , Maybe(..)
                                                )
import           Data.Semigroup                 ( Endo
                                                , (<>)
                                                )
import           Obelisk.Route.Frontend         (Routed,  R
                                                , RouteToUrl
                                                , SetRoute
                                                , routeLink
                                                )
import           Page.Common                    (getStatsLocalAndRemote,  elNotImplemented
                                                , elCongraz
                                                , loading
                                                )
import           Palantype.Common               ( kiChordsStart
                                                , Lang(DE)
                                                , renderPlover
                                                , kiBackUp
                                                , Chord
                                                , Palantype
                                                , RawSteno
                                                )
import           Reflex.Dom                     (current, gate, Dynamic,  Performable
                                                , PerformEvent
                                                , TriggerEvent
                                                , holdUniqDyn
                                                , updated
                                                , EventWriter
                                                , switchDyn
                                                , widgetHold
                                                , holdDyn
                                                , never
                                                , performEvent
                                                , Event
                                                , (=:)
                                                , DomBuilder
                                                , MonadHold
                                                , PostBuild
                                                , Prerender
                                                , blank
                                                , delay
                                                , dyn_
                                                , el
                                                , elAttr
                                                , elClass
                                                , foldDyn
                                                , getPostBuild
                                                , text
                                                )
import           State                          ( State
                                                , Env(..)
                                                , Navigation(..)
                                                , stageUrl
                                                )
import           TextShow                       ( TextShow(showt) )
import           Text.Read                      ( readMaybe )
import           Obelisk.Generated.Static       ( static )
import           Palantype.Common.TH            ( readLoc )
import           Control.Category               ( (<<<)
                                                , (.)
                                                )
import           Common.Stage                   (Stage,  stageMeta )
import           Data.Text                      ( Text )
import           Data.Map.Strict                ( Map )
import qualified Palantype.Common.Indices      as KI
import           Control.Monad.Random           ( getRandomR
                                                , newStdGen
                                                , MonadRandom
                                                , evalRand
                                                )
import           Data.Time                      ( defaultTimeLocale
                                                , Day(ModifiedJulianDay)
                                                , Day
                                                )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Safe                           ( initMay )
import           Data.Traversable               ( Traversable(sequence) )
import           Data.Eq                        ( Eq((==)) )
import qualified Data.Text                     as Text
import           GHC.Num                        ( Num((+)) )
import           Data.Foldable                  ( Foldable(null) )
import           Data.Functor                   ( Functor(fmap) )
import           Data.Functor                   ( Functor((<$)) )
import           Data.List                      ( take )
import qualified Palantype.Common.RawSteno     as Raw
import           Control.Monad                  ( unless )
import           Control.Lens                   ( (<>~)
                                                , (.~)
                                                , (%~)
                                                , (+~)
                                                )
import           Data.Function                  ( (&) )
import           Data.Foldable                  ( Foldable(elem) )
import           Shared                         ( dynSimple )
import qualified Data.Time                     as Time
import           Common.Model                   ( Stats )
import           GHC.Generics                   ( Generic )
import Data.Bool (Bool, not)
import Data.Tuple (fst, snd)
import Data.List (filter)

data StateDates k
    = StatePause Int
    | StateRun (Run k)
    deriving (Generic)

data Run key = Run
    { stCounter :: Int
    , stChords  :: [Chord key]
    , stDates   :: [Day]
    , stNMistakes :: Int
    } deriving (Generic)

taskDates
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
    -> m (Event t Stats)
taskDates dynStats evChord map = do

    eStdGen <- postRender $ do
        ePb <- getPostBuild
        performEvent $ ePb $> liftIO newStdGen

    dynMStdGen <- holdDyn Nothing $ Just <$> eStdGen

    dynSimple $ dynMStdGen <&> maybe
        (pure never)
        \stdGen -> do
            let step :: Chord key -> StateDates key -> StateDates key
                step c st = case st of
                    StatePause _ ->
                        if Raw.fromChord c
                                `elem` (KI.toRaw @key <$> kiChordsStart)
                            then stepStart
                            else st
                        -- let current = _stDates !! _stCounter
                    StateRun Run {..}
                        | Raw.fromChord c == KI.toRaw @key kiBackUp ->
                        -- undo last input
                                                                       case
                                initMay stChords
                            of
                                Just cs ->
                                    st
                                        &  _As @"StateRun"
                                        %~ (field @"stChords" .~ cs)
                                        .  (field @"stNMistakes" +~ 1)
                                Nothing -> st

                    StateRun Run {..} ->
                        if renderDate (stDates !! stCounter)
                            == renderPlover map (stChords <> [c])
                        then -- correct? next!
                            if stCounter + 1 == numDates
                                then StatePause stNMistakes
                                else
                                    st
                                    &  _As @"StateRun"
                                    %~ (field @"stCounter" +~ 1)
                                    .  (field @"stChords" .~ [])
                        else -- incorrect? keep going.
                            st & _As @"StateRun" . field @"stChords" <>~ [c]

                stepStart = StateRun Run
                    { stCounter   = 0
                    , stChords    = []
                    , stDates     = evalRand getRandomDates stdGen
                    , stNMistakes = 0
                    }

            dynStenoDates <- foldDyn step (StatePause 0) evChord

            evStartStop <- fmap updated $ holdUniqDyn $ dynStenoDates <&> \case
                StatePause nMistakes -> nMistakes
                StateRun   _         -> -1

            dynStopwatch <- mkStopwatch evStartStop

            elClass "div" "taskWords" $ do
                dyn_ $ dynStenoDates <&> \case
                    StatePause _ -> el "div" $ do
                        text "Type "
                        elClass "span" "btnSteno blinking" $ do
                            text "Start "
                            el "code" $ text "SDAÜD"
                        text " to begin the exercise."
                    StateRun Run {..} -> do
                        elClass "span" "word"
                            $  elClass "div" "exerciseField multiline"
                            $  el "code"
                            $  text
                            $  renderDate
                            $  stDates
                            !! stCounter

                        elClass "span" "input"
                            $  text
                            $  renderPlover map stChords
                            <> " …"

                        el "span" $ do
                            elClass "span" "btnSteno" $ text $ "↤ " <> showt
                                (KI.toRaw @key kiBackUp) -- U+21A4
                            elClass "span" "small" $ text $ if null stChords
                                then " to show hint"
                                else " to back up"

                        elClass "hr" "visibilityHidden" blank

                        el "strong" $ text $ showt stCounter
                        text $ " / " <> showt numDates

                elStopwatch dynStats dynStopwatch numDates

getRandomDates :: MonadRandom m => m [Day]
getRandomDates =
    sequence $ take numDates $ repeat $ ModifiedJulianDay <$> getRandomR
        (0, 60000)

renderDate :: Day -> Text
renderDate d = Text.pack $ Time.formatTime defaultTimeLocale "%d.%m.%Y" d

numDates :: Int
numDates = 100
numberMode
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
numberMode = mdo
    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang == DE) elNotImplemented

    el "h1" $ text "Typing numbers"

    el "h2" $ text "Palantype number mode"

    el "h3" $ text "Digits and related symbols"

    elClass "div" "paragraph" $ do
        text
            "For typing numbers, the virtual keyboard above can assist \
             \you quite a bit. Just hold down "
        el "code" $ text "WN-"
        text " and you can see, how to reach numbers and related symbols."

    elClass "div" "paragraph" $ elAttr
        "img"
        (  "src"
        =: $(static "numbermode.png")
        <> "alt"
        =: "Keyboard layout in number mode"
        )
        blank

    elClass "div" "paragraph" $ do
        text
            "Note how, apart from the digits 0-9 for the fingers of your \
             \right hand, the extra keys for the thumbs allow to input \
             \even longer numbers all at once, in particular common dates \
             \like "
        el "em" $ text "1990"
        text ", or "
        el "em" $ text "2022"
        text "."

    elClass "div" "paragraph" $ do
        text
            "Also, the input of common shortcuts that involve numbers \
             \is possible by adding a modifier key to any input. \
             \The available modifiers are "
        el "code" $ text "Control"
        text ", "
        el "code" $ text "Super"
        text ", and "
        el "code" $ text "Alt"
        text ". "
        el "code" $ text "Super"
        text " is usually called the Windows-key."

    el "h3" $ text "The special characters of number mode"

    elClass "div" "paragraph"
        $ text
              "Following the standard US keyboard layout, you can reach \
             \special characters using the Shift modifier key in combination \
             \with a number key. \
             \The virtual keyboard assists you here again."

    elClass "div" "paragraph" $ elAttr
        "img"
        (  "src"
        =: $(static "numbermode-shift.png")
        <> "alt"
        =: "Keyboard layout in number mode"
        )
        blank

    elClass "div" "paragraph" $ do
        text
            "Note that access to these special chars via the number mode \
             \shouldn't be usually necessary when typing regularly. \
             \Rather, consider them part of the \
             \extended finger spelling. \
             \For the usual formatting, the "
        let stagePloverCommands = $readLoc "stage_ploverCommands"
        routeLink (stageUrl navLang stagePloverCommands)
            $  text
            $  "Exercise "
            <> showt (stageMeta stagePloverCommands)
        text " should be all you ever need."

    elClass "div" "paragraph" $ do
        text
            "Also, there are special characters missing. In number mode, \
             \there are only those special characters that you reach via \
             \the Shift modifier plus some number. The remaining \
             \special characters can be found in "
        let stageSpecialCharacters = $readLoc "stage_specialCharacters"
        routeLink (stageUrl navLang stageSpecialCharacters)
            $  text
            $  "Exercise "
            <> showt (stageMeta stageSpecialCharacters)
        text "."

    el "h3" $ text "Practicing dates"

    elClass "div" "paragraph" $ do
        text
            "Feel free to practice dates here. The format is fairly common \
             \in Germany and you will learn numbers just fine this way. \
             \There are alternative ways to reach the same output now: \
             \Feel free to type digit by digit or use as many fingers as \
             \possible at once."

    dynStatsAll <- getStatsLocalAndRemote evDone
    evPb <- postRender $ delay 0.1 =<< getPostBuild
    evEDict <- request $ getDictDENumbers evPb
    evDone <- fmap switchDyn $ widgetHold (loading $> never) $ evEDict <&> \case
        Left str -> never <$ elClass
            "span"
            "red small"
            (text $ "Couldn't load resource: " <> str)
        Right map -> taskDates dynStatsAll (gate (not <$> current dynDone) envEChord) map

    let dynStatsPersonal = fmap snd . filter (isNothing . fst) . fmap snd <$> dynStatsAll
    dynDone <- elCongraz (Just <$> evDone) dynStatsPersonal envNavigation
    pure envNavigation
