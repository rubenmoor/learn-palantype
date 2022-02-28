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

import Client (getDictDENumbers, postRender, request)
import Common.Route (FrontendRoute)
import Control.Applicative (Applicative (pure))
import Control.Monad ((=<<))
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader.Class (MonadReader, ask)
import Data.Bool (Bool (..))
import Data.Either (Either (..))
import Data.Foldable (for_)
import Data.Function (($))
import Data.Functor (($>), (<&>))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List ((!!), repeat)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybe, fromMaybe, Maybe (..))
import Data.Semigroup (Endo, (<>))
import Obelisk.Route.Frontend (R, RouteToUrl, SetRoute, routeLink)
import Page.Common (elNotImplemented, elCongraz, loading)
import Palantype.Common (Lang (DE), renderPlover, kiBackUp, fromChord, Chord, Palantype, RawSteno)
import Reflex.Dom (EventWriter, switchDyn, widgetHold, updated, holdUniqDyn, holdDyn, never, performEvent, Event, (=:), DomBuilder, MonadHold, PostBuild, Prerender, blank, delay, dyn_, el, elAttr, elClass, foldDyn, getPostBuild, text)
import Shared (dynSimple)
import State (State, Env (..), Navigation (..), stageUrl)
import TextShow (TextShow (showt))
import Text.Read (readMaybe)
import Text.Show (Show(show))
import Obelisk.Generated.Static (static)
import Palantype.Common.TH (readLoc)
import Control.Category ((<<<))
import Common.Stage (stageMeta, StageMeta(..))
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Palantype.Common.Indices as KI
import Control.Monad.Random (getRandomR, newStdGen, MonadRandom, evalRand)
import Data.Time (defaultTimeLocale, formatTime, Day(ModifiedJulianDay), Day)
import Control.Monad.Reader (asks)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Safe (initMay, atMay)
import System.Random.Shuffle (shuffleM)
import Data.Traversable (Traversable(sequence))
import Data.Eq (Eq((==)))
import GHC.Enum (pred, Enum(succ))
import qualified Data.Text as Text
import GHC.Num (Num((+)))
import Control.Monad (when)
import Data.Ord (Ord((<)))
import Data.List (intersperse)
import Data.Foldable (Foldable(null))
import Data.Functor (void)
import Control.Category (Category(id))
import Data.Witherable (Filterable(filter))
import Data.Functor (Functor(fmap))
import Data.Functor (Functor((<$)))
import Data.List (take)
import qualified Palantype.Common.RawSteno as Raw
import Control.Monad (unless)

numberMode ::
    forall key t (m :: * -> *).
    ( DomBuilder t m
    , EventWriter t (Endo State) m
    , MonadFix m
    , MonadHold t m
    , MonadReader (Env t key) m
    , Palantype key
    , PostBuild t m
    , Prerender t m
    , RouteToUrl (R FrontendRoute) m
    , SetRoute t (R FrontendRoute) m
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

    elCongraz eDone envNavigation
    pure envNavigation

data StenoDatesState key = StenoDatesState
    { sdsCounter :: Int
    , sdsDone    :: Bool
    , sdsChords  :: [Chord key]
    , sdsDates   :: [Day]
    , sdsNMistakes :: Int
    }

taskDates ::
    forall key t (m :: * -> *).
    ( DomBuilder t m
    , MonadFix m
    , MonadHold t m
    , MonadReader (Env t key) m
    , Palantype key
    , PostBuild t m
    , Prerender t m
    ) =>
    Map RawSteno Text ->
    m (Event t ())
taskDates map = do
    eChord <- asks envEChord

    eStdGen <- postRender $ do
        ePb <- getPostBuild
        performEvent $ ePb $> liftIO newStdGen

    dynMStdGen <- holdDyn Nothing $ Just <$> eStdGen

    dynSimple $
        dynMStdGen <&> \case
            Nothing -> pure never
            Just stdGen -> do

                let
                    step :: Chord key -> StenoDatesState key -> StenoDatesState key
                    step chord ls@StenoDatesState {..} =
                        case sdsDates `atMay` sdsCounter of

                            -- reset after done
                            Nothing ->
                                let dates' = evalRand getRandomDates stdGen
                                 in ls { sdsDone = False
                                       , sdsCounter = 0
                                       , sdsDates = dates'
                                       }

                            -- undo last input
                            _ | Raw.fromChord chord == KI.toRaw @key kiBackUp ->
                                case initMay sdsChords of
                                    Just cs ->
                                        ls { sdsChords = cs
                                           , sdsNMistakes = succ sdsNMistakes
                                           }
                                    Nothing -> ls

                            Just date ->
                                if renderDate date == renderPlover map (sdsChords <> [chord])
                                    then -- correct? next!
                                        ls { sdsDone = sdsCounter == pred numDates
                                           , sdsCounter = sdsCounter + 1
                                           , sdsChords = []
                                           }
                                    else -- incorrect? keep going.
                                        ls { sdsChords = sdsChords <> [chord] }
                    stepInitial =
                        StenoDatesState
                            { sdsCounter = 0
                            , sdsChords = []
                            , sdsDone = False
                            , sdsDates = evalRand getRandomDates stdGen
                            , sdsNMistakes = 0
                            }

                dynStenoDates <- foldDyn step stepInitial eChord

                dynDone <- holdUniqDyn $ sdsDone <$> dynStenoDates
                let eDone = updated dynDone

                elClass "div" "taskWords"
                    $ dyn_
                    $ dynStenoDates <&> \StenoDatesState {..} -> do
                        elClass "span" "word"
                            $ when (sdsCounter < numDates)
                            $ elClass "div" "exerciseField multiline"
                            $ el "code"
                            $ text
                            $ renderDate
                            $ sdsDates !! sdsCounter

                        elClass "span" "input"
                            $ text $ renderPlover map sdsChords <> "…"

                        el "span" $ do
                            elClass "span" "btnSteno" $ text $
                                "↤ "
                                    <> showt
                                        (KI.toRaw @key kiBackUp) -- U+21A4
                            elClass "span" "small" $ text $
                                if null sdsChords
                                    then " to show hint"
                                    else " to back up"

                let dynCounter = sdsCounter <$> dynStenoDates
                dyn_ $ dynCounter <&> \c ->
                    elClass "div" "paragraph" $ do
                        el "strong" $ text $ showt c
                        text " / "
                        text $ showt numDates

                dyn_ $ dynDone <&> \bDone ->
                    when bDone $ elClass "div" "small anthrazit" $
                        text "Cleared. Press any key to start over."

                pure $ void $ filter id eDone

getRandomDates :: MonadRandom m => m [Day]
getRandomDates =
    sequence
        $ take numDates
        $ repeat
        $ ModifiedJulianDay <$> getRandomR (0, 60000)

renderDate :: Day -> Text
renderDate d = Text.pack $ formatTime defaultTimeLocale "%d.%m.%Y" d

numDates :: Int
numDates = 100
