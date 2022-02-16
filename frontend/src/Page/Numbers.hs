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

module Page.Numbers where

--

import Client (getDictDENumbers, getDocDEPatternAll, postRender, request)
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

    el "h1" $ text "Typing numbers"

    evPb <- postRender $ delay 0.1 =<< getPostBuild
    evEDict <- request $ getDictDENumbers evPb

    widgetHold_ loading $
        evEDict <&> \case
            Left str -> elClass "span" "red small" $ text $ "Couldn't load resource: " <> str
            Right map -> do
              blank

    pure envNavigation
