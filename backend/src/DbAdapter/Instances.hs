{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DbAdapter.Instances where

import Common.Model (Rank, TextLang, CacheContentType)
import Palantype.Common (StageRepr, SystemLang)
import Database.Persist.TH (derivePersistFieldJSON)

derivePersistFieldJSON "Rank"
derivePersistFieldJSON "StageRepr"

derivePersistFieldJSON "TextLang"

derivePersistFieldJSON "SystemLang"

derivePersistFieldJSON "CacheContentType"
