{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Page.Stage15.PloverCommands
    ( ploverCommands
    ) where

import           CMS                            ( elCMS
                                                , elCMSContent
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader.Class     ( MonadReader )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( Maybe(..) )
import           Data.Semigroup                 ( Endo )
import           Reflex.Dom                     ( DomBuilder
                                                , EventWriter
                                                , MonadHold
                                                , PerformEvent(..)
                                                , PostBuild
                                                , Prerender(..)
                                                , TriggerEvent
                                                )
import           State                          ( Env(..)
                                                , State
                                                )
import           Witherable                     ( Filterable(..) )

ploverCommands
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
ploverCommands = do
    evContent <- elCMS 1 <&> mapMaybe \case
        [p] -> Just p
        _   -> Nothing

    elCMSContent evContent
