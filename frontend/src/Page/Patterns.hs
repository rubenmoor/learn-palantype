{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Page.Patterns where

--

import Reflex.Dom (blank, elClass, el, text, EventWriter, DomBuilder)
import Control.Monad.Reader.Class (ask, MonadReader)
import Palantype.Common (toDescription, Palantype)
import Obelisk.Route.Frontend (R, SetRoute)
import Common.Route (FrontendRoute)
import Data.Semigroup (Endo(Endo))
import State (State (..), Env (..), Navigation (..))
import Palantype.DE (patternDoc)
import Data.Traversable (for)
import Data.Foldable (for_)
import TextShow (TextShow(showt))

overview
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadReader (Env t key) m
       , Palantype key
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
overview = do
  Env {..} <- ask

  el "h1" $ text "Pattern group overview"

  for_ patternDoc $ \(p, lsPattern) -> do
    el "h2" $ text $ toDescription p
    elClass "div" "patternTable" $ do
    for_ lsPattern $ \(g, pairs) -> do
      el "h3" $ text $ "Greediness " <> showt g
      for_ pairs $ \(orig, steno) ->
        elClass "div" "floatLeft" $ do
          elClass "span" "orig" $ text orig
          elClass "code" "steno" $ text $ showt steno
      elClass "br" "clearBoth" blank

  pure envNavigation
