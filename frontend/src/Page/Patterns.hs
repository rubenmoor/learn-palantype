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

module Page.Patterns where

--

import Client (getDocDEPatternAll, postRender, request)
import Common.Route (FrontendRoute)
import Control.Applicative (Applicative (pure))
import Control.Monad ((=<<))
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader.Class (MonadReader, ask)
import Data.Bool (Bool (..), bool, not)
import Data.Either (Either (..))
import Data.Foldable (for_)
import Data.Function (($))
import Data.Functor ((<&>))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (zip)
import qualified Data.Map.Strict as Map
import Data.Maybe (Maybe (..))
import Data.Semigroup ((<>))
import Obelisk.Route.Frontend (R, RouteToUrl, SetRoute, routeLink)
import Page.Common (elPatterns, loading)
import Palantype.Common (toDescription)
import Reflex.Dom ((=:), DomBuilder, EventName (Click), MonadHold, PostBuild, Prerender, blank, delay, domEvent, dynText, dyn_, el, elAttr, elClass, elClass', elDynClass, foldDyn, getPostBuild, text, widgetHold_)
import Shared (iFa)
import State (Env (..), Navigation (..), stageUrl)
import TextShow (TextShow (showt))
import Text.Read (readMaybe)
import Text.Show (Show(show))

overview ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      PostBuild t m,
      Prerender t m,
      RouteToUrl (R FrontendRoute) m,
      SetRoute t (R FrontendRoute) m
    ) =>
    m Navigation
overview = do
    Env {..} <- ask
    let Navigation {..} = envNavigation

    el "h1" $ text "Pattern group overview"

    ePb <- postRender $ delay 0.1 =<< getPostBuild
    eEDoc <- request $ getDocDEPatternAll ePb

    widgetHold_ loading $
        eEDoc <&> \case
            Left str -> elClass "span" "red small" $ text $ "Couldn't load resource: " <> str
            Right (patternDoc, m) -> do
                -- TOC embedded in content
                let iPatternDoc = zip [1 ..] patternDoc
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
                        el "li" $ elAttr "a" ("href" =: ("#" <> showt p))
                            $ text
                            $ showt @Int i <> " " <> toDescription p

                -- pattern documentation content
                for_ iPatternDoc \(i, (p, lsPattern)) -> do
                    elAttr "h2" ("id" =: showt p)
                        $ elAttr "a" ("href" =: ("#" <> showt p))
                        $ text
                        $ showt i <> " " <> toDescription p
                    el "div" $ for_ lsPattern $ \(g, doc) -> do
                        let (n, lsExamples) =
                                Map.findWithDefault (0, []) g $ Map.findWithDefault Map.empty p m
                        el "h3" $ do
                            text $ "Greediness " <> showt g
                            case readMaybe $ "stage_" <> show p <> "_" <> show g of
                                Just stage -> routeLink (stageUrl navLang stage) $ iFa "fas fa-book-open"
                                Nothing -> elClass "span" "lightgray" $ iFa "fas fa-book-open"

                        elClass "div" "patternExamples" $ mdo
                            elClass "strong" "floatLeft" $ text "Examples"
                            (btn, _) <- elClass' "button" "floatLeft" $ dyn_ $
                                dynToggle <&> \case
                                    False -> iFa "fas fa-plus"
                                    True -> iFa "fas fa-minus"
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
