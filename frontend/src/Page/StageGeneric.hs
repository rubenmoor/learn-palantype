{-# LANGUAGE TemplateHaskell #-}
module Page.StageGeneric
  ( getGenericExercise
  ) where

import qualified Page.Stage3 as Stage3
import qualified Page.Stage4 as Stage4
import qualified Page.Stage5 as Stage5
import qualified Page.Stage6 as Stage6
import qualified Page.Stage7 as Stage7
import qualified Page.Stage8 as Stage8
import qualified Page.Stage9 as Stage9
import qualified Page.Stage10 as Stage10
import qualified Page.Stage11 as Stage11
import qualified Page.Stage12 as Stage12
import qualified Page.Stage13 as Stage13
import qualified Page.Stage14 as Stage14
import Data.Map.Strict (Map)
import qualified Palantype.DE as DE
import Palantype.Common (Greediness)
import Page.Common.Exercise (exercise, Constraints)
import Common.Stage (StageHierarchy)
import State (Navigation)
import qualified Data.Map.Strict as Map
import Palantype.Common.TH (failure)

getGenericExercise
  :: forall key t (m :: * -> *)
  . Constraints key t m
  => (DE.Pattern, Greediness)
  -> StageHierarchy
  -> m Navigation
  -> m Navigation
getGenericExercise pgg h elNotImplemented = case Map.lookup pgg exercises of
    Nothing -> elNotImplemented
    Just (part1, part2) -> exercise h part1 pgg part2

exercises
  :: forall key t (m :: * -> *)
  . Constraints key t m
  => Map (DE.Pattern, Greediness) (m (), m ())
exercises = Map.unionsWith (\_ _ -> $failure "key duplicate")
    [ Stage3.exercises
    , Stage4.exercises
    , Stage5.exercises
    , Stage6.exercises
    , Stage7.exercises
    , Stage8.exercises
    , Stage9.exercises
    , Stage10.exercises
    , Stage11.exercises
    , Stage12.exercises
    , Stage13.exercises
    , Stage14.exercises
    ]
