{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DbAdapter.Instances where

import Common.Model (Rank)
import Common.Stage (StageRepr)
import Database.Persist.TH (derivePersistFieldJSON)

derivePersistFieldJSON "Rank"
derivePersistFieldJSON "StageRepr"
