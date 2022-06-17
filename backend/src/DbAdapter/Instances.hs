{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DbAdapter.Instances where

import Common.Model (Rank)
import Database.Persist.TH (derivePersistFieldJSON)

derivePersistFieldJSON "Rank"
