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

import           CMS                            ( elCMS )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader.Class     ( MonadReader )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>)
                                                , (<&>)
                                                )
import           Data.Maybe                     ( Maybe(..) )
import           Reflex.Dom                     ( DomBuilder
                                                , MonadHold
                                                , PerformEvent(..)
                                                , PostBuild
                                                , Prerender(..)
                                                , TriggerEvent
                                                , blank
                                                , widgetHold_
                                                )
import           Reflex.Dom.Pandoc              ( defaultConfig
                                                , elPandoc
                                                )
import           State                          ( Env(..) )
import           Witherable                     ( Filterable(..) )

ploverCommands
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
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

    widgetHold_ blank $ elPandoc defaultConfig <$> evContent
