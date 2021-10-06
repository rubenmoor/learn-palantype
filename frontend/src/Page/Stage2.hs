{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Page.Stage2 where

import           Common.Route           (FrontendRoute (..))
import           Control.Applicative    (Applicative (pure))
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.Reader   (MonadReader (ask))
import           Obelisk.Route.Frontend (R, RouteToUrl,
                                         SetRoute (setRoute))
import           Reflex.Dom             (DomBuilder, EventWriter,
                                         MonadHold (holdDyn),
                                         PostBuild (getPostBuild), Prerender)
import           State                  (EStateUpdate, Env (..),
                                         Navigation (..))

exercise1
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Env t) m
  , Prerender js t m
  , PostBuild t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  )
  => m Navigation
exercise1 = do
  Env {..} <- ask
  pure envNavigation
