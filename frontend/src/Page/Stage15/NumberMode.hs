{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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

module Page.Stage15.NumberMode
    ( numberMode
    )
where

import           Data.Generics.Sum              ( _As )
import           Data.Generics.Product          ( field )
import           Page.Common.Stopwatch          ( elStopwatch
                                                , mkStopwatch
                                                )
import           Client                         ( postRender
                                                )
import           Common.Route                   ( FrontendRoute )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.Reader.Class     ( MonadReader
                                                , ask
                                                )
import           Data.Either                    ( Either(..) )
import Data.Function ( ($), (&) )
import Data.Functor ( ($>), (<&>), (<$>), Functor(fmap) )
import           Data.Int                       ( Int )
import Data.List ( (!!), filter )
import           Data.Maybe                     (isNothing,  maybe
                                                , Maybe(..)
                                                )
import           Data.Semigroup                 ( Endo
                                                , (<>)
                                                )
import           Obelisk.Route.Frontend         (R
                                                , SetRoute
                                                )
import           Page.Common                    (getStatsLocalAndRemote
                                                , elCongraz, chordStart
                                                )
import           Palantype.Common               ( renderPlover
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
                                                , holdDyn
                                                , never
                                                , performEvent
                                                , Event
                                                , DomBuilder
                                                , MonadHold
                                                , PostBuild
                                                , Prerender
                                                , blank
                                                , dyn_
                                                , el
                                                , elClass
                                                , foldDyn
                                                , getPostBuild
                                                , text
                                                )
import           State                          ( State
                                                , Env(..)

                                                )
import           TextShow                       ( TextShow(showt) )
import           Control.Category               ( (.)
                                                )
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
import           Data.Eq                        ( Eq((==)) )
import qualified Data.Text                     as Text
import           GHC.Num                        ( Num((+)) )
import qualified Palantype.Common.RawSteno     as Raw
import           Control.Monad                  ( replicateM )
import           Control.Lens                   ( (<>~)
                                                , (.~)
                                                , (%~)
                                                , (+~)
                                                )
import           Shared                         ( dynSimple )
import qualified Data.Time                     as Time
import           Common.Model                   ( Stats )
import           GHC.Generics                   ( Generic )
import Data.Bool (Bool, not)
import Data.Tuple (fst, snd)
import PloverDict (eMapNumbersForExercise)
import CMS (elCMS, elCMSContent)
import Witherable (Filterable(mapMaybe))
import Data.Foldable (Foldable(null))

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
                        if c == chordStart
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
    replicateM numDates (ModifiedJulianDay <$> getRandomR (0, 60000))

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
       , SetRoute t (R FrontendRoute) m
       , TriggerEvent t m
       )
    => m ()
numberMode = mdo
    Env {..} <- ask

    evContent <- elCMS 1 <&> mapMaybe \case
      [c] -> Just c
      _   -> Nothing

    elCMSContent evContent

    dynStatsAll <- getStatsLocalAndRemote evDone
    evDone <- case eMapNumbersForExercise @key of
        Left str  -> do
          elClass "p" "small red" $ text $ "Couldn't load resource: " <> str
          pure never
        Right map ->
          taskDates dynStatsAll (gate (not <$> current dynDone) envEChord) map

    let dynStatsPersonal = fmap snd . filter (isNothing . fst) . fmap snd <$> dynStatsAll
    dynDone <- elCongraz (Just <$> evDone) dynStatsPersonal envNavigation
    blank
