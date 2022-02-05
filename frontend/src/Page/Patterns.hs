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

import Reflex.Dom (dynText, (=:), elAttr, dyn_, PostBuild, EventName(Click), domEvent, elClass', foldDyn, elDynClass, MonadHold, widgetHold_, Prerender, delay, getPostBuild, blank, elClass, el, text, DomBuilder)
import Control.Monad.Reader.Class (ask, MonadReader)
import Palantype.Common (toDescription)
import Data.Semigroup ((<>))
import State (stageUrl, Env (..), Navigation (..))
import Data.Foldable (for_)
import TextShow (TextShow(showt))
import Data.Function (($))
import Control.Applicative (Applicative(pure))
import Client (postRender, request, getDocDEPatternAll)
import Shared (iFa)
import Control.Monad ((=<<))
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Bool (bool, Bool (..), not)
import Control.Monad.Fix (MonadFix)
import Data.Functor ((<$>))
import Page.Common (elPatterns, loading)
import Data.Either (Either(..))
import Data.List (zip)
import Data.Int (Int)
import Obelisk.Route.Frontend (SetRoute, R, RouteToUrl, routeLink)
import Common.Route (FrontendRoute)
import Common.Stage (fromStringMaybe)
import Data.Maybe (Maybe(..))

overview
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , PostBuild t m
       , Prerender t m
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
overview = do
  Env {..} <- ask
  let Navigation {..} = envNavigation

  el "h1" $ text "Pattern group overview"

  ePb     <- postRender $ delay 0.1 =<< getPostBuild
  eEDoc <- request $ getDocDEPatternAll ePb

  widgetHold_ loading $ eEDoc <&> \case
    Left str -> elClass "span" "red small" $ text $ "Couldn't load resource: " <> str
    Right (patternDoc, m) -> do

      -- TOC embedded in content
      let iPatternDoc = zip [1 .. ] patternDoc
      elClass "div" "embeddedToc" $ mdo
        dynToggle <- foldDyn (\_ -> not) False (domEvent Click elToggle)
        elToggle <- el "div" $ do
          el "strong" $ text "Contents"
          text " ("
          (elT, _) <- elClass' "a" "normalLink" $ dynText $ bool "show" "hide" <$> dynToggle
          text ")"
          pure elT
        let dynCls = bool "displayNone" "block" <$> dynToggle
        elDynClass "ul" dynCls $ for_ iPatternDoc \(i, (p, _)) -> do
          el "li" $ elAttr "a" ("href" =: ("#" <> showt p)) $
            text $ showt @Int i <> " " <> toDescription p

      -- pattern documentation content
      for_ iPatternDoc \(i, (p, lsPattern)) -> do

        elAttr "h2" ("id" =: showt p) $
          elAttr "a" ("href" =: ("#" <> showt p)) $
            text $ showt i <> " " <> toDescription p
        el "div" $ for_ lsPattern $ \(g, doc) -> do
          let (n, lsExamples) =
                Map.findWithDefault (0, []) g $ Map.findWithDefault Map.empty p m
          el "h3" $ do
            text $ "Greediness " <> showt g
            case fromStringMaybe $ "stage_" <> showt p <> "-" <> showt g of
              Just stage -> routeLink (stageUrl navLang stage) $ iFa "fas fa-book-open"
              Nothing    -> elClass "span" "lightgray" $ iFa "fas fa-book-open"

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

          elPatterns doc

  pure envNavigation
