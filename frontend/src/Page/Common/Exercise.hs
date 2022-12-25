{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Page.Common.Exercise
    ( Constraints
    , exercise
    ) where

import Common.Route (FrontendRoute)
import Common.Stage (StageIndex)
import Control.Monad (unless)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import Data.Semigroup (Endo)
import Obelisk.Route (R)
import Obelisk.Route.Frontend (Routed, RouteToUrl, SetRoute)
import Page.Common (getStatsLocalAndRemote, elCongraz, elNotImplemented, elPatterns, taskWords)
import Palantype.Common (patternDoc, Greediness, Lang (..), Palantype, toDescription)
import Palantype.DE (Pattern (..))
import Reflex.Dom (current, gate, TriggerEvent, DomBuilder, EventWriter, MonadHold, PerformEvent, Performable, PostBuild, Prerender, el, elClass, never, text)
import State (Env (..), Navigation (..), State)
import TextShow (TextShow (showt))
import Data.Maybe (isNothing)
import qualified Data.Map.Strict as Map
import PloverDict (getMapsForExercise)

type Constraints key t m =
    ( DomBuilder t m
    , EventWriter t (Endo State) m
    , MonadFix m
    , MonadHold t m
    , MonadReader (Env t key) m
    , Palantype key
    , MonadIO (Performable m)
    , PerformEvent t m
    , PostBuild t m
    , Prerender t m
    , Routed t StageIndex m
    , RouteToUrl (R FrontendRoute) m
    , SetRoute t (R FrontendRoute) m
    , TriggerEvent t m
    )

exercise
  :: forall key t (m :: * -> *)
  .  Constraints key t m
  => Int
  -> Int
  -> m ()
  -> Pattern
  -> Greediness
  -> m ()
  -> m Navigation
exercise iStage iEx elIntro pat greediness elExplication = mdo
    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang == DE) elNotImplemented

    el "h1" $ text $ "Stage " <> showt iStage
    el "h3" $ text $ "Exercise " <> showt iEx
    el "h2" $ text $ toDescription pat
    el "h4" $ text $ "Greediness " <> showt greediness

    elIntro

    elPatterns
        $ Map.toList
        $ Map.findWithDefault Map.empty greediness
        $ Map.findWithDefault Map.empty pat patternDoc

    elExplication

    dynStatsAll <- getStatsLocalAndRemote evDone

    evDone      <- case getMapsForExercise pat greediness of
        Left str -> do
            elClass "p" "small red" $ text $ "Couldn't load exercise: " <> str
            pure never
        Right (mSW, mWSs) -> taskWords
            dynStatsAll
            (gate (not <$> current dynDone) envEChord)
            mSW
            mWSs

    let dynStatsPersonal = fmap snd . filter (isNothing . fst) . fmap snd <$> dynStatsAll
    dynDone <- elCongraz (Just <$> evDone) dynStatsPersonal envNavigation
    pure envNavigation
