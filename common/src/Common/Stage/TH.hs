{-# LANGUAGE TemplateHaskell #-}

module Common.Stage.TH where

import Language.Haskell.TH (Exp, Q)
import Debug.Trace.LocationTH (failure)
import Text.Read (readMaybe)

readLoc :: Q Exp
readLoc =
  [| \str -> case readMaybe str of
        Just x  -> x
        Nothing -> $failure $ "read failed: " <> str
  |]
