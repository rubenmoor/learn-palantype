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

import           CMS                            ( elCMS )
import           Common.Route                   ( FrontendRoute(..) )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( (.) )
import           Control.Lens                   ( (%~)
                                                , (.~)
                                                )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader           ( MonadReader(ask) )
import           Data.Eq                        ( Eq((==)) )
import           Data.Function                  ( ($)
                                                )
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
                                                , SetRoute(setRoute)
                                                )
import           Palantype.Common               ( Palantype
                                                , SystemLang(..)
                                                )
import           Palantype.Common.RawSteno      ( parseChordMaybe )
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



                                                , el
                                                , elAttr
                                                , elClass
                                                , elClass'


                                                , leftmost

                                                , text
                                                , widgetHold_
                                                )
import           Reflex.Dom.Pandoc              ( defaultConfig
                                                , elPandoc
                                                )
import           State                          ( Env(..)
                                                , Navigation(..)
                                                , State
                                                , stageUrl
                                                , updateState
                                                )
import           Witherable                     ( Filterable(filter, mapMaybe)
                                                )

introduction
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
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
    => m Navigation
introduction = do

    Env {..} <- ask
    let
        Navigation {..} = envNavigation

    evParts <- elCMS 3 <&> mapMaybe \case
      [p1, p2, p3] -> Just (p1, p2, p3)
      _ -> Nothing

    widgetHold_ blank $ evParts <&> \(part1, part2, part3) -> do
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

        let rsStart = case navSystemLang of
                SystemEN -> "START"
                SystemDE -> "DSAÜD"
            eChordSTART =
                void $ filter (== $fromJust (parseChordMaybe rsStart)) envEChord

        elClass "div" "start" $ do
            (btn, _) <- elClass' "button" "small" $ text "Get Started!"
            let eStart = leftmost [eChordSTART, domEvent Click btn]
            updateState
                $  eStart
                $> [ field @"stApp" . field @"stProgress" %~ Map.insert navSystemLang ($fromJust navMNext)
                  , field @"stApp" . field @"stCleared" %~ Set.insert navCurrent
                  , field @"stApp" . field @"stTOCShowStage" .~ Set.singleton 1
                  ]
            setRoute $ eStart $> stageUrl @key 1

        elPandoc defaultConfig part3

    pure envNavigation
