{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE BlockArguments #-}

module Page.Introduction where

import           CMS                            ( elCMS, elCMSContent )
import           Common.Route                   ( FrontendRoute(..) )
import           Control.Category               ( (.) )
import           Control.Lens                   ( (%~)
                                                , (.~)
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader           ( MonadReader(ask) )
import           Data.Eq                        ( Eq((==)) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( ($>)
                                                , (<&>)
                                                , void
                                                )
import           Data.Generics.Product          ( field )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( Maybe(..) )
import           Data.Semigroup                 ( Endo(..)
                                                , Semigroup((<>))
                                                )
import qualified Data.Set                      as Set
import           Obelisk.Route.Frontend         ( R
                                                , SetRoute
                                                )
import           Page.Common                    ( chordStart )
import           Palantype.Common               ( Palantype )
import           Palantype.Common.TH            ( fromJust )
import           Reflex.Dom                     ( (=:)
                                                , DomBuilder
                                                , EventName(Click)
                                                , EventWriter
                                                , HasDomEvent(domEvent)
                                                , MonadHold
                                                , PerformEvent(Performable)
                                                , PostBuild
                                                , Prerender
                                                , TriggerEvent
                                                , blank
                                                , elAttr
                                                , elClass
                                                , elClass'
                                                , leftmost
                                                , text
                                                , splitE, el
                                                )
import           State                          ( Env(..)
                                                , Navigation(..)
                                                , State
                                                , stageUrl
                                                , updateState
                                                )
import           Witherable                     ( Filterable(filter, mapMaybe) )
import Data.Bifunctor (Bifunctor(first))
import Shared (setRouteAndLoading)

introduction
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadIO (Performable m)
       , MonadReader (Env t key) m
       , Palantype key
       , PerformEvent t m
       , Prerender t m
       , PostBuild t m
       , SetRoute t (R FrontendRoute) m
       , TriggerEvent t m
       )
    => m ()
introduction = do

    Env {..} <- ask
    let
        Navigation {..} = envNavigation

    evCMS <- elCMS 3 <&> mapMaybe \case
      [p1, p2, p3] -> Just ((p1, p2), p3)
      _            -> Nothing
    let ((evPart1, evPart2), evPart3) = first splitE $ splitE evCMS

    elCMSContent evPart1

    elClass "p" "textAlign-center" $ elAttr
        "iframe"
        (  "width"
        =: "640"
        <> "height"
        =: "480"
        <> "src"
        =: "https://www.youtube.com/embed/za1qxU4jdfg"
        )
        blank

    elCMSContent evPart2

    let eChordSTART = void $ filter (== Just chordStart) envEvMChord

    -- prose class: to center within prose text-flow
    elClass "div" "my-prose" $ elClass "div" "mx-auto my-4 steno-action" $ do
        (btn, _) <- elClass' "button" "" $ text "Get Started!"
        let eStart = leftmost [eChordSTART, domEvent Click btn]
        updateState $  eStart $>
          [ field @"stApp" . field @"stToc" . field @"stProgress"  %~ Map.insert navSystemLang ($fromJust navMNext)
          , field @"stApp" . field @"stToc" . field @"stCleared"   %~ Set.insert navCurrent
          , field @"stApp" . field @"stToc" . field @"stShowStage" .~ Set.singleton 1
          ]
        setRouteAndLoading $ eStart $> stageUrl @key 1

    elCMSContent evPart3
