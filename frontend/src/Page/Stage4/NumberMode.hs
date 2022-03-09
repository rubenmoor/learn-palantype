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
    ) where

import           Page.Common.Stopwatch
import Client (getDictDENumbers, postRender, request)
import Common.Route (FrontendRoute)
import Control.Applicative (Applicative (pure))
import Control.Monad ((=<<))
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader.Class (MonadReader, ask)
import Data.Either (Either (..))
import Data.Function (($))
import Data.Functor (($>), (<&>))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List ((!!), repeat)
import Data.Maybe (maybe, Maybe (..))
import Data.Semigroup (Endo, (<>))
import Obelisk.Route.Frontend (R, RouteToUrl, SetRoute, routeLink)
import Page.Common (elNotImplemented, elCongraz, loading)
import Palantype.Common (kiChordsStart, Lang (DE), renderPlover, kiBackUp, Chord, Palantype, RawSteno)
import Reflex.Dom (Performable, PerformEvent, TriggerEvent, holdUniqDyn, updated, EventWriter, switchDyn, widgetHold, holdDyn, never, performEvent, Event, (=:), DomBuilder, MonadHold, PostBuild, Prerender, blank, delay, dyn_, el, elAttr, elClass, foldDyn, getPostBuild, text)
import State (Stats, State, Env (..), Navigation (..), stageUrl)
import TextShow (TextShow (showt))
import Text.Read (readMaybe)
import Obelisk.Generated.Static (static)
import Palantype.Common.TH (readLoc)
import Control.Category ((<<<), (.))
import Common.Stage (stageMeta)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Palantype.Common.Indices as KI
import Control.Monad.Random (getRandomR, newStdGen, MonadRandom, evalRand)
import Data.Time (defaultTimeLocale, Day(ModifiedJulianDay), Day)
import Control.Monad.Reader (asks)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Safe (initMay)
import Data.Traversable (Traversable(sequence))
import Data.Eq (Eq((==)))
import qualified Data.Text as Text
import GHC.Num (Num((+)))
import Data.Foldable (Foldable(null))
import Data.Functor (Functor(fmap))
import Data.Functor (Functor((<$)))
import Data.List (take)
import qualified Palantype.Common.RawSteno as Raw
import Control.Monad (unless)
import Control.Lens ((<>~), (.~), (%~), (+~), makePrisms, makeLenses)
import Data.Function ((&))
import Data.Foldable (Foldable(elem))
import Shared (dynSimple)
import Data.Bool (Bool(..))
import qualified Data.Time as Time

data StateDates k
    = StatePause
    | StateRun (Run k)

data Run key = Run
    { _stCounter :: Int
    , _stChords  :: [Chord key]
    , _stDates   :: [Day]
    , _stNMistakes :: Int
    }

makePrisms ''StateDates
makeLenses ''Run

taskDates
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
    -> m (Event t Stats)
taskDates map = do
    eChord <- asks envEChord

    eStdGen <- postRender $ do
        ePb <- getPostBuild
        performEvent $ ePb $> liftIO newStdGen

    dynMStdGen <- holdDyn Nothing $ Just <$> eStdGen

    dynSimple $ dynMStdGen <&> maybe (pure never) \stdGen -> do
        let
            step :: Chord key -> StateDates key -> StateDates key
            step c st = case st of
                StatePause ->
                    if Raw.fromChord c `elem` (KI.toRaw @key <$> kiChordsStart)
                        then stepStart
                        else st
                    -- let current = _stDates !! _stCounter
                StateRun Run {..}
                    | Raw.fromChord c == KI.toRaw @key kiBackUp ->
                    -- undo last input
                    case initMay _stChords of
                        Just cs ->
                            st & _StateRun %~ ( stChords    .~ cs )
                                            . ( stNMistakes +~ 1  )
                        Nothing -> st

                StateRun Run {..} ->
                    if renderDate (_stDates !! _stCounter) == renderPlover map (_stChords <> [c])
                        then -- correct? next!
                            if _stCounter + 1 == numDates
                                then StatePause
                                else st & _StateRun %~ ( stCounter +~ 1  )
                                                     . ( stChords  .~ [] )
                        else -- incorrect? keep going.
                            st & _StateRun . stChords <>~ [c]

            stepStart = StateRun Run
                    { _stCounter = 0
                    , _stChords = []
                    , _stDates = evalRand getRandomDates stdGen
                    , _stNMistakes = 0
                    }

        dynStenoDates <- foldDyn step StatePause eChord

        evStartStop <-
            fmap updated $ holdUniqDyn $ dynStenoDates <&> \case
                StatePause -> False
                StateRun _ -> True

        dynStopwatch <- mkStopwatch evStartStop

        elClass "div" "taskWords" $ do
            dyn_ $ dynStenoDates <&> \case
                StatePause -> el "div" $ do
                    text "Type "
                    elClass "span" "btnSteno blinking" $ do
                        text "Start "
                        el "code" $ text "SDAÜD"
                    text " to begin the exercise."
                StateRun Run {..} -> do
                    elClass "span" "word"
                        $ elClass "div" "exerciseField multiline"
                        $ el "code" $ text $ renderDate $ _stDates !! _stCounter

                    elClass "span" "input"
                        $ text $ renderPlover map _stChords <> " …"

                    el "span" $ do
                        elClass "span" "btnSteno" $
                            text $ "↤ " <> showt (KI.toRaw @key kiBackUp) -- U+21A4
                        elClass "span" "small" $ text $
                            if null _stChords
                                then " to show hint"
                                else " to back up"

                    elClass "hr" "visibilityHidden" blank

                    el "strong" $ text $ showt _stCounter
                    text $ " / " <> showt numDates

            elStopwatch dynStopwatch numDates

getRandomDates :: MonadRandom m => m [Day]
getRandomDates =
    sequence
        $ take numDates
        $ repeat
        $ ModifiedJulianDay <$> getRandomR (0, 60000)

renderDate :: Day -> Text
renderDate d = Text.pack $ Time.formatTime defaultTimeLocale "%d.%m.%Y" d

numDates :: Int
numDates = 100
numberMode ::
    forall key t (m :: * -> *).
    ( DomBuilder t m
    , EventWriter t (Endo State) m
    , MonadFix m
    , MonadHold t m
    , MonadIO (Performable m)
    , MonadReader (Env t key) m
    , Palantype key
    , PerformEvent t m
    , PostBuild t m
    , Prerender t m
    , RouteToUrl (R FrontendRoute) m
    , SetRoute t (R FrontendRoute) m
    , TriggerEvent t m
    ) =>
    m Navigation
numberMode = do
    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang == DE) elNotImplemented

    el "h1" $ text "Typing numbers"

    el "h2" $ text "Palantype number mode"

    el "h3" $ text "Digits and related symbols"

    elClass "div" "paragraph" $ do
        text "For typing numbers, the virtual keyboard above can assist \
             \you quite a bit. Just hold down "
        el "code" $ text "WN-"
        text " and you can see, how to reach numbers and related symbols."

    elClass "div" "paragraph" $
        elAttr "img" (  "src" =: $(static "numbermode.png")
                     <> "alt" =: "Keyboard layout in number mode"
                     ) blank

    elClass "div" "paragraph" $ do
        text "Note how, apart from the digits 0-9 for the fingers of your \
             \right hand, the extra keys for the thumbs allow to input \
             \even longer numbers all at once, in particular common dates \
             \like "
        el "em" $ text "1990"
        text ", or "
        el "em" $ text "2022"
        text "."

    elClass "div" "paragraph" $ do
        text "Also, the input of common shortcuts that involve numbers \
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

    elClass "div" "paragraph" $
        text "Following the standard US keyboard layout, you can reach \
             \special characters using the Shift modifier key in combination \
             \with a number key. \
             \The virtual keyboard assists you here again."

    elClass "div" "paragraph" $
        elAttr "img" (  "src" =: $(static "numbermode-shift.png")
                     <> "alt" =: "Keyboard layout in number mode"
                     ) blank

    elClass "div" "paragraph" $ do
        text "Note that access to these special chars via the number mode \
             \shouldn't be usually necessary when typing regularly. \
             \Rather, consider them part of the \
             \extended finger spelling. \
             \For the usual formatting, the "
        let stagePloverCommands = $readLoc "stage_ploverCommands"
        routeLink (stageUrl navLang stagePloverCommands) $
            text $ "Exercise " <> showt (stageMeta stagePloverCommands)
        text " should be all you ever need."

    elClass "div" "paragraph" $ do
        text "Also, there are special characters missing. In number mode, \
             \there are only those special characters that you reach via \
             \the Shift modifier plus some number. The remaining \
             \special characters can be found in "
        let stageSpecialCharacters = $readLoc "stage_specialCharacters"
        routeLink (stageUrl navLang stageSpecialCharacters) $
            text $ "Exercise " <> showt (stageMeta stageSpecialCharacters)
        text "."

    el "h3" $ text "Practicing dates"

    elClass "div" "paragraph" $ do
        text "Feel free to practice dates here. The format is fairly common \
             \in Germany and you will learn numbers just fine this way. \
             \There are alternative ways to reach the same output now: \
             \Feel free to type digit by digit or use as many fingers as \
             \possible at once."

    evPb <- postRender $ delay 0.1 =<< getPostBuild
    evEDict <- request $ getDictDENumbers evPb
    eDone <- fmap switchDyn $ widgetHold (loading $> never) $
        evEDict <&> \case
            Left  str -> never <$
                elClass "span" "red small"
                        (text $ "Couldn't load resource: " <> str)
            Right map -> taskDates map

    elCongraz (Just <$> eDone) envNavigation
    pure envNavigation
