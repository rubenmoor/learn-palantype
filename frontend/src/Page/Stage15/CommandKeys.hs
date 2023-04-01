{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Page.Stage15.CommandKeys
    ( commandKeys
    ) where

import           CMS                            ( elCMS
                                                , elCMSContent
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader           ( MonadReader )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( Maybe(..) )
import           Data.Monoid                    ( Endo )
import           Reflex.Dom                     ( DomBuilder
                                                , EventWriter
                                                , MonadHold
                                                , PerformEvent(..)
                                                , PostBuild
                                                , Prerender
                                                , TriggerEvent
                                                )
import           State                          ( Env(..)
                                                , State
                                                )
import           Witherable                     ( Filterable(mapMaybe) )

commandKeys
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
commandKeys = do
    evContent <- elCMS 1 <&> mapMaybe \case
        [c] -> Just c
        _   -> Nothing

    elCMSContent evContent
