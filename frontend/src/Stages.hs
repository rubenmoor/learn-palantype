{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stages
    ( stages
    ) where

import Palantype.Common (Palantype)
import Common.Stage (StageSpecialGeneric(..), StageHierarchy(StageSublevel, StageToplevel), Stage(Stage))
import Type.Reflection (eqTypeRep, (:~~:)(HRefl), typeRep)
import qualified Palantype.DE as DE
import qualified Palantype.EN as EN

stages
  :: forall key
  . ( Palantype key
    ) => [Stage key]
stages =
    [ Stage (StageSpecial "introduction"  ) StageToplevel
    , Stage (StageSpecial "stage"         ) (StageSublevel 1 1)
    , Stage (StageSpecial "stage"         ) (StageSublevel 1 2)
    , Stage (StageSpecial "stage"         ) (StageSublevel 1 3)
    , Stage (StageSpecial "stage"         ) (StageSublevel 1 4)
    , Stage (StageSpecial "stage"         ) (StageSublevel 1 5)
    , Stage (StageSpecial "stage"         ) (StageSublevel 1 6)
    , Stage (StageSpecial "stage"         ) (StageSublevel 1 7)
    , Stage (StageSpecial "stage"         ) (StageSublevel 1 8)
    , Stage (StageSpecial "stage"         ) (StageSublevel 2 1)
    , Stage (StageSpecial "stage"         ) (StageSublevel 2 2)
    ] <> if
        | Just HRefl <- typeRep @key `eqTypeRep` typeRep @DE.Key -> stagesDE
        | Just HRefl <- typeRep @key `eqTypeRep` typeRep @EN.Key -> [] -- TODO
        | otherwise -> []

stagesDE
  :: [Stage DE.Key]
stagesDE =
    [ Stage (StageSpecial "stage"            ) (StageSublevel 2 3)
    , Stage (StageGeneric DE.PatSimple      0) (StageSublevel 2 4)
    , Stage (StageGeneric DE.PatSimpleMulti 0) (StageSublevel 2 5)
    , Stage (StageGeneric DE.PatReplCommon1 0) (StageSublevel 3 1)
    , Stage (StageGeneric DE.PatReplCommon2 0) (StageSublevel 3 2)
    , Stage (StageGeneric DE.PatCodaComboT  0) (StageSublevel 3 3)
    , Stage (StageGeneric DE.PatReplCommon1 2) (StageSublevel 4 1)
    , Stage (StageGeneric DE.PatReplCommon1 3) (StageSublevel 4 2)
    , Stage (StageGeneric DE.PatReplCommon2 4) (StageSublevel 4 3)
    , Stage (StageGeneric DE.PatOnsetR      0) (StageSublevel 5 1)
    , Stage (StageGeneric DE.PatOnsetL      0) (StageSublevel 5 2)
    , Stage (StageGeneric DE.PatDiConsonant 0) (StageSublevel 5 3)
    , Stage (StageGeneric DE.PatDiConsonant 2) (StageSublevel 5 4)
    , Stage (StageGeneric DE.PatCodaH       0) (StageSublevel 6 1)
    , Stage (StageGeneric DE.PatCodaH       1) (StageSublevel 6 2)
    , Stage (StageGeneric DE.PatCodaR       0) (StageSublevel 6 3)
    , Stage (StageGeneric DE.PatCodaR       4) (StageSublevel 6 4)
    , Stage (StageGeneric DE.PatCodaRR      0) (StageSublevel 6 5)
    , Stage (StageGeneric DE.PatCodaHR      0) (StageSublevel 6 6)

    -- , Stage (StageGeneric PatDt          0)
    -- , Stage (StageGeneric PatDiphtong    0)
    -- , Stage (StageGeneric PatReplC       0)
    -- , Stage (StageGeneric PatBreakUpI    0)
    -- , Stage (StageGeneric PatSwapS       0)
    -- , Stage (StageGeneric PatSwapSch     0)
    -- , Stage (StageGeneric PatSwapZ       0)
    -- , Stage (StageGeneric PatDiVowel     0)
    -- , Stage (StageGeneric PatReplH       0)
    -- , Stage (StageGeneric PatCodaGK      3)
    -- , Stage (StageGeneric PatReplRare    0)  -- 3.23
    -- , Stage (StageGeneric PatSmallS      0)
    , Stage (StageSpecial "ploverCommands"   ) (StageSublevel 40 1)
    , Stage (StageSpecial "fingerspelling"   ) (StageSublevel 40 2)
    , Stage (StageSpecial "numbermode"       ) (StageSublevel 40 3)
    , Stage (StageSpecial "commandKeys"      ) (StageSublevel 40 4)
    , Stage (StageSpecial "specialCharacters") (StageSublevel 40 5)
    , Stage (StageGeneric DE.PatBrief 0      ) (StageSublevel 50 1)
    , Stage (StageSpecial "patternoverview"  ) StageToplevel
    ]
