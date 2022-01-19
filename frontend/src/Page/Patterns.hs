{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}

module Page.Patterns where

--

import Reflex.Dom (dyn_, PostBuild, EventName(Click), domEvent, elClass', foldDyn, elDynClass, MonadHold, widgetHold_, Prerender, delay, getPostBuild, (=:), elAttr, blank, elClass, el, text, EventWriter, DomBuilder)
import Control.Monad.Reader.Class (ask, MonadReader)
import Palantype.Common (toDescription, Palantype)
import Obelisk.Route.Frontend (R, SetRoute)
import Common.Route (FrontendRoute)
import Data.Semigroup ((<>), Endo)
import State (State (..), Env (..), Navigation (..))
import Data.Foldable (for_)
import TextShow (TextShow(showt))
import Data.Text (toLower)
import Data.Function (($))
import Data.Text (length)
import Data.Ord (Ord((>)))
import GHC.Real (fromIntegral, Fractional((/)))
import Data.Monoid (Monoid(mempty))
import GHC.Float (Double)
import Control.Applicative (Applicative(pure))
import GHC.Num ((+))
import Client (request, getDocDEPatterns, RequestResult (..), postRender)
import Shared (iFa)
import Control.Monad ((=<<))
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Bool (bool, Bool (..), not)
import Control.Monad.Fix (MonadFix)
import Data.Functor ((<$>))
import Page.Common (loading)

overview
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
overview = do
  Env {..} <- ask

  el "h1" $ text "Pattern group overview"

  ePb     <- postRender $ delay 0.1 =<< getPostBuild
  RequestResult {..} <- request $ getDocDEPatterns ePb

  widgetHold_ blank $ rrEFailure <&> \str ->
    elClass "span" "red small" $ text str

  widgetHold_ loading $ rrESuccess <&> \(patternDoc, m) -> for_ patternDoc \(p, lsPattern) -> do
    el "h2" $ text $ toDescription p
    elClass "div" "patternTable" $ do
    for_ lsPattern $ \(g, lsPPosPairs) -> do
      let (n, lsExamples) =
            Map.findWithDefault (0, []) g $ Map.findWithDefault Map.empty p m
      el "h3" $ text $ "Greediness " <> showt g

      elClass "div" "patternExamples" $ mdo
        elClass "strong" "floatLeft" $ text "Examples"
        (btn, _) <- elClass' "button" "floatLeft" $ dyn_ $ dynToggle <&> \case
          False -> iFa "fas fa-plus"
          True  -> iFa "fas fa-minus"
        elClass "em" "floatRight" $ do
          text "# total: "
          text $ showt n
        elClass "br" "clearBoth" blank

        dynToggle <- foldDyn (\_ -> not) False $ domEvent Click btn
        let dynShow = bool "whiteSpaceNoWrap" "" <$> dynToggle
        elDynClass "div" dynShow $ for_ lsExamples $ \(w, s) -> do
          el "span" $ text w
          text " "
          el "code" $ text $ showt s
          text ", "

      for_ lsPPosPairs $ \(pPos, pairs) -> do
        let strPPos = toLower $ showt pPos
        elClass "hr" strPPos blank
        elClass "span" ("patternPosition " <> strPPos) $ text strPPos
        elClass "br" "clearBoth" blank
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
