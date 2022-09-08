{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
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

module PloverDict
  ( eMapDictExamples
  , getMapsForExercise
  , eMapNumbersForExercise
  ) where

import Control.Applicative (Applicative (pure))
import Data.Bool (otherwise)
import Data.Either (Either (..))
import Data.Function (($))
import Data.Int (Int)
import qualified Data.Map.Strict as Map
import Data.Semigroup ((<>))
import Palantype.DE (pDE)
import Palantype.EN (pEN)
import Palantype.Common (RawSteno, Greediness, PatternGroup, Palantype)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Data.Proxy (Proxy(Proxy))
import Data.Eq (Eq((==)))
import Data.Typeable (typeRep)
import Control.Lens (over, _Left)
import Obelisk.Generated.Static (staticFileContent)
import Data.Tuple (snd)
import Data.Foldable (Foldable(foldl'))

eMapDictExamples
  :: forall key
  .  Palantype key
  => Either Text (Map (PatternGroup key) (Map Greediness (Int, [(Text, RawSteno)])))
eMapDictExamples =
  let
      dtKey = typeRep (Proxy :: Proxy key)
      keyDE = typeRep pDE
      keyEN = typeRep pEN
  in  do
        bsDocMap <- if
            | dtKey == keyDE -> Right $(staticFileContent "palantype-DE-doc.json")
            | dtKey == keyEN -> Left  "Documentation: Examples: EN: not implemented"
            | otherwise      -> Left  "Documentation: Examples: language not implemented"

        over _Left (\str -> "Documentation: Examples: could not decode file: " <> Text.pack str) $
          Aeson.eitherDecodeStrict bsDocMap

getMapsForExercise
  :: forall key
  .  Palantype key
  => PatternGroup key
  -> Greediness
  -> Either Text (Map RawSteno Text, Map Text [RawSteno])
getMapsForExercise p g = do
  map <- eMapDictExamples
  let
      ls = snd $ Map.findWithDefault (0, []) g
               $ Map.findWithDefault Map.empty p map
  pure $
    foldl' (\(mSW, mWSs) (w, s) ->
              ( Map.insert s w mSW
              , Map.insertWith (<>) w [s] mWSs
              )
           ) (Map.empty, Map.empty) ls

eMapNumbersForExercise
  :: forall key
  .  Palantype key
  => Either Text (Map RawSteno Text)
eMapNumbersForExercise =
  let
      dtKey = typeRep (Proxy :: Proxy key)
      keyDE = typeRep pDE
      keyEN = typeRep pEN
  in  do
        bsNumbersMap <- if
            | dtKey == keyDE -> Right $(staticFileContent "palantype-DE-numbers.json")
            | dtKey == keyEN -> Left  "Documentation: Numbers: EN: not implemented"
            | otherwise      -> Left  "Documentation: Numbers: language not implemented"

        over _Left (\str -> "Documentation: Numbers: could not decode file: " <> Text.pack str) $
          Aeson.eitherDecodeStrict bsNumbersMap
