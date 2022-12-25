{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Stages
    ( stages
    ) where

import Palantype.Common (Palantype)
import Common.Stage (StageSpecialGeneric(..), StageHierarchy(StageSublevel, StageToplevel), Stage(Stage))
import Type.Reflection (eqTypeRep, (:~~:)(HRefl), typeRep)
import qualified Palantype.DE as DE
import qualified Palantype.EN as EN
import Obelisk.Frontend (ObeliskWidget)
import Obelisk.Route.Frontend (RoutedT, R)
import Common.Route (FrontendRoute)
import State (Env, State, Navigation)
import Reflex.Dom (EventWriterT)
import Control.Monad.Reader (ReaderT)
import Data.Semigroup (Endo)

stages
  :: forall key
  . ( Palantype key
    ) => [Stage key]
