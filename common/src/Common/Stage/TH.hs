{-# LANGUAGE TemplateHaskell #-}

module Common.Stage.TH where

import Language.Haskell.TH (Exp, Q)
import Debug.Trace.LocationTH (failure)
import Text.Read (readMaybe)

{-
   like Prelude.read but relying on TemplateHaskell to provide
   location information (file/line number) upon failure.
  
   `readLoc` is similar to `$Debug.Trace.LocationTH.check read`,
   but uses `readMaybe` and does not need `unsafePerformIO`.
-}
readLoc :: Q Exp
readLoc =
  [| \str -> case readMaybe str of
        Just x  -> x
        Nothing -> $failure $ "read failed: " <> str
  |]
