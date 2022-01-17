{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Page.Patterns where

--

import Reflex.Dom (MonadHold, widgetHold_, Prerender, delay, getPostBuild, (=:), elAttr, blank, elClass, el, text, EventWriter, DomBuilder)
import Control.Monad.Reader.Class (ask, MonadReader)
import Palantype.Common (PatternGroup, toDescription, Palantype)
import Obelisk.Route.Frontend (R, SetRoute)
import Common.Route (FrontendRoute)
import Data.Semigroup ((<>), Endo(Endo))
import State (State (..), Env (..), Navigation (..))
import Data.Traversable (for)
import Data.Foldable (for_)
import TextShow (TextShow(showt))
import qualified Data.Text as Text
import Data.Text (toLower)
import Data.Function (($))
import Data.Text (length)
import Data.Ord (Ord((>)))
import GHC.Real (fromIntegral, Fractional((/)))
import Data.Monoid (Monoid(mempty))
import GHC.Float (Double)
import Control.Applicative (Applicative(pure))
import GHC.Num ((+), (-), Num((*)))
import Client (request, getDocDEPatterns, RequestResult (..), postRender)
import Shared (iFa)
import Control.Monad ((=<<))
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Tuple (fst)

overview
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadReader (Env t key) m
       , MonadHold t m
       , Palantype key
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
overview = do
  Env {..} <- ask

  el "h1" $ text "Pattern group overview"

  ePb     <- postRender $ delay 0.1 =<< getPostBuild
  RequestResult {..} <- request $ getDocDEPatterns ePb

  let loading = do
        iFa "fas fa-spinner fa-spin"
        text " Loading ..."

  widgetHold_ blank $ rrEFailure <&> \str ->
    elClass "span" "red small" $ text str

  widgetHold_ loading $ rrESuccess <&> \(patternDoc, m) -> for_ patternDoc \(p, lsPattern) -> do
    el "h2" $ text $ toDescription p
    elClass "div" "patternTable" $ do
    for_ lsPattern $ \(g, lsPPosPairs) -> do
      let (n, lsExamples) =
            Map.findWithDefault (0, []) g $ Map.findWithDefault Map.empty p m
      el "h3" $ text $ "Greediness " <> showt g
      elClass "span" "" $ do
        text "# total: "
        text $ showt n
      for_ lsPPosPairs $ \(pPos, pairs) -> do
        let strPPos = toLower $ showt pPos
        elClass "hr" strPPos blank
        elClass "span" ("patternPosition " <> strPPos) $ text strPPos
        for_ pairs $ \(orig, steno) ->
          elClass "div" "floatLeft" $ do
            let
                lOrig :: Double = fromIntegral $ length orig
                styleOrig =
                  if lOrig > 6
                    then "style" =: ("font-size: " <> showt ((1 + 6 / lOrig) / 2) <> "em")
                    else mempty
                lSteno :: Double = fromIntegral $ length $ showt steno
                styleSteno =
                  if lSteno > 6
                    then "style" =: ("font-size: " <> showt (6 / lSteno) <> "em")
                    else mempty
            elAttr "div" ("class" =: "orig" <> styleOrig) $ text orig
            elAttr "code" ("class" =: "steno" <> styleSteno) $ text $ showt steno
        elClass "br" "clearBoth" blank

  pure envNavigation
