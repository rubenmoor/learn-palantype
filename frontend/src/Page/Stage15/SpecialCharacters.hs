{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Page.Stage15.SpecialCharacters
    ( specialCharacters
    ) where

import Control.Monad.Reader.Class (MonadReader)
import Reflex.Dom (DomBuilder, EventWriter, TriggerEvent, MonadHold, PerformEvent (..), PostBuild, Prerender)
import State (Env (..), State)
import CMS (elCMSContent, elCMS)
import Witherable (Filterable(mapMaybe))
import Data.Functor ((<&>))
import Data.Maybe (Maybe(..))
import Data.Semigroup (Endo)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)

specialCharacters
  :: forall key t (m :: * -> *)
  . ( DomBuilder t m
    , EventWriter t (Endo State) m
    , MonadFix m
    , MonadHold t m
    , MonadIO (Performable m)
    , MonadReader (Env t key) m
    , PerformEvent t m
    , PostBuild t m
    , Prerender t m
    , TriggerEvent t m
    )
  => m ()
specialCharacters = do
    -- Env {..} <- ask

    evContent <- elCMS 1 <&> mapMaybe \case
      [c] -> Just c
      _   -> Nothing

    elCMSContent evContent
