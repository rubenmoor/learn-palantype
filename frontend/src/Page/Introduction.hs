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
import           Control.Lens                   ((%~)
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
import           Palantype.Common               ( SystemLang(..)
                                                , Palantype
                                                )
import           Palantype.Common.RawSteno      ( parseChordMaybe )
import           Reflex.Dom                     (constDyn, fanEither, widgetHold_, MonadHold, Prerender, PostBuild, getPostBuild, Event,  elClass'
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
import           Palantype.Common.TH            ( fromJust )
import Data.Functor ((<$>))
import Data.Functor ((<&>))
import Shared (loadingScreen)
import Data.Functor (Functor(fmap))
import Client (getCMS, request)
import Servant.Common.Req (QParam(QParamSome))
import Common.Model (TextLang(TextEN))
import Reflex.Dom.Pandoc (defaultConfig, elPandoc)

introduction
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , Prerender t m
       , PostBuild t m
       , SetRoute t (R FrontendRoute) m
       )
    => (Event t () -> Event t ())
    -> m Navigation
introduction toReady = do

    Env {..} <- ask
    evReady <- toReady <$> getPostBuild

    let
        Navigation {..} = envNavigation

        systemLang = QParamSome SystemDE
        txtLang    = QParamSome TextEN
        pageName   = QParamSome "introduction"

    (evRespFail, evRespSucc) <- fmap fanEither $ request $
      getCMS (constDyn systemLang)
             (constDyn txtLang   )
             (constDyn pageName  )
             evReady

    widgetHold_ (loadingScreen "") $ evRespFail <&> \strError ->
      el "span" $ text $ "Error loading content: " <> strError

    widgetHold_ (loadingScreen "") $ evRespSucc <&> \case
        [part1, part2, part3] -> do

            elPandoc defaultConfig part1

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

            elPandoc defaultConfig part2

            let rsStart = case navLang of
                    SystemEN -> "START"
                    SystemDE -> "DSAÜD"
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

            elPandoc defaultConfig part3
        _ -> el "span" $ text "wrong number of parts in markdown"

    pure envNavigation
