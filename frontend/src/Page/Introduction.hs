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

module Page.Introduction where

import           Common.Route                   ( FrontendRoute(..) )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( (.)
                                                )
import           Control.Lens                   ((?~), at,  (%~)
                                                , (.~)
                                                )
import           Control.Monad.Reader           ( MonadReader(ask) )
import           Data.Eq                        ( Eq((==)) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( ($>)
                                                , void
                                                )
import           Data.Generics.Product          ( field )
import qualified Data.Map                      as Map
import           Data.Semigroup                 ( Endo(..)
                                                , Semigroup((<>))
                                                )
import qualified Data.Set                      as Set
import           Data.Witherable                ( Filterable(filter) )
import           Obelisk.Route.Frontend         ( R
                                                , SetRoute(setRoute)
                                                )
import           Palantype.Common               ( Lang(..)
                                                , Palantype
                                                )
import           Palantype.Common.RawSteno      ( parseChordMaybe )
import           Reflex.Dom                     (xhrRequestConfig_headers, XhrResponse (..), widgetHold_, MonadHold, widgetHold, Prerender, Performable, TriggerEvent, PostBuild, getPostBuild, Client, PerformEvent, XhrRequest(..), def, Event, performRequestAsync,  elClass'
                                                , (=:)
                                                , DomBuilder
                                                , EventName(Click)
                                                , EventWriter
                                                , HasDomEvent(domEvent)
                                                , blank
                                                , el
                                                , elAttr
                                                , elClass
                                                , leftmost
                                                , text
                                                )
import           State                          ( stageUrl
                                                , Env(..)
                                                , Navigation(..)
                                                , State
                                                , updateState
                                                )
import           TextShow                       ( TextShow(showt) )
import           Palantype.Common.TH            ( fromJust )
import Data.Functor ((<$>))
import Language.Javascript.JSaddle (MonadJSM)
import Data.Functor ((<&>))
import Control.Lens ((&))
import Data.Text (Text)

import Shared (loadingScreen)
import Client (postRender)
import Github
    (GithubAPIResponse (..),  RequestConfig(RequestConfig, rqcLang, rqcWrittenLang,
                    rqcPageName),
      requestGithub )
import qualified Data.Text as Text

introduction
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , Prerender t m
       , PostBuild t (Event t)
       , SetRoute t (R FrontendRoute) m
       )
    => (Event t () -> Event t ())
    -> m Navigation
introduction toReady = do

    Env {..} <- ask

    let
        Navigation {..} = envNavigation
        eReady = toReady <$> getPostBuild

        rqcLang = "PalantypeDE"
        rqcWrittenLang = "EN"
        rqcPageName = "introduction"
        reqConfig = RequestConfig{..}

        -- TODO retrieve file from github

    eContent <- postRender $ requestGithub (eReady $> reqConfig)
    widgetHold_ (loadingScreen "") $ eContent <&> \case
        GithubAPIError code str -> el "span" $
            text $ "Error loading content: " <> showt code <> " " <> str
        GithubAPISuccess str -> el "span" $ text $ Text.take 500 str

    -- TODO part1

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

    -- TODO part2

    let rsStart = case navLang of
            EN -> "START"
            DE -> "DSAÜD"
        eChordSTART =
            void $ filter (== $fromJust (parseChordMaybe rsStart)) envEChord

    elClass "div" "start" $ do
        (btn, _) <- elClass' "button" "small" $ text "Get Started!"
        let eStart = leftmost [eChordSTART, domEvent Click btn]
        updateState
            $  eStart
            $> [ field @"stApp" . field @"stProgress" %~ Map.insert navLang ($fromJust navMNext)
               , field @"stApp" . field @"stCleared" %~ Set.insert navCurrent
               , field @"stApp" . field @"stTOCShowStage" .~ Set.singleton 1
               ]
        setRoute $ eStart $> stageUrl @key 1

    -- TODO part3

    pure envNavigation
