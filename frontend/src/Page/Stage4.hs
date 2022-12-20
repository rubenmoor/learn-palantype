{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Page.Stage4 where

import           Common.Route                   ( FrontendRoute )
import           Common.Stage                   ( Stage
                                                , StageMeta(..)
                                                , stageMeta
                                                )
import           Control.Category               ( (<<<) )
import           Control.Monad                  ( unless )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader.Class     ( MonadReader
                                                , ask
                                                )
import           Data.Semigroup                 ( Endo )
import           Obelisk.Route                  ( R )
import           Obelisk.Route.Frontend         ( Routed
                                                , RouteToUrl
                                                , SetRoute
                                                , routeLink
                                                )
import           Page.Common                    ( getStatsLocalAndRemote
                                                , elCongraz
                                                , elNotImplemented
                                                , elPatterns
                                                , taskWords
                                                )
import Page.Common.Exercise (Constraints, exercise)
import           Palantype.Common               ( patternDoc
                                                , Lang(..)
                                                , Palantype
                                                , toDescription
                                                )
import           Palantype.Common.TH            ( failure
                                                , readLoc
                                                )
import           Palantype.DE                   ( Pattern(..) )
import           Reflex.Dom                     ( blank
                                                , current
                                                , gate
                                                , TriggerEvent
                                                , DomBuilder
                                                , EventWriter
                                                , MonadHold
                                                , PerformEvent
                                                , Performable
                                                , PostBuild
                                                , Prerender
                                                , el
                                                , elClass
                                                , never
                                                , text
                                                )
import           State                          ( Env(..)
                                                , Navigation(..)
                                                , State
                                                , stageUrl
                                                )
import           Text.Read                      ( readMaybe )
import           TextShow                       ( TextShow(showt) )
import           Data.Maybe                     ( isNothing )
import qualified Data.Map.Strict               as Map
import           PloverDict                     ( getMapsForExercise )

exercise1 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
exercise1 = exercise
    4
    1
    (\_ ->
        el "p"
            $ text
                  "todo."
    )
    PatReplCommon1
    3
    (\navLang -> do
        el "p" $ do
            text
                "todo"
    )

exercise2 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
exercise2 = exercise
    4
    2
    (\_ ->
        el "p"
            $ text
                  "todo."
    )
    PatReplCommon1
    4
    (\navLang -> do
        el "p" $ do
            text
                "todo"
    )

exercise3 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
exercise3 = exercise
    4
    3
    (\_ ->
        el "p"
            $ text
                  "todo."
    )
    PatReplCommon2
    3
    (\navLang -> do
        el "p" $ do
            text
                "todo"
    )

exercise4 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
exercise4 = exercise
    4
    4
    (\_ ->
        el "p"
            $ text
                  "todo."
    )
    PatCodaComboT
    3
    (\navLang -> do
        el "p" $ do
            text
                "todo"
    )
