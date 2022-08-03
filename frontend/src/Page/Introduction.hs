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
import Control.Category ((<<<), (.))
import           Control.Lens                   ( (%~)
                                                , (.~)
                                                )
import           Control.Monad.Reader           ( MonadReader(ask) )
import           Data.Bool                      ( Bool(..) )
import           Data.Eq                        ( Eq((==)) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( ($>)
                                                , void
                                                )
import           Data.Generics.Product          ( field )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( Maybe(Just) )
import           Data.Semigroup                 ( Endo(..)
                                                , Semigroup((<>))
                                                )
import qualified Data.Set                      as Set
import           Data.Witherable                ( Filterable(filter) )
import           Obelisk.Route.Frontend         ( R
                                                , SetRoute(setRoute)
                                                )
import           Palantype.Common               (Lang(..),  Palantype )
import           Palantype.Common.RawSteno      ( parseChordMaybe )
import           Reflex.Dom                     (elClass',  (=:)
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
import           State                          (stageUrl,  Env(..)
                                                , Navigation(..)
                                                , State
                                                , updateState
                                                )
import Text.Read (readMaybe)
import           TextShow                       ( TextShow(showt) )
import Palantype.Common.TH (readLoc)
import Palantype.Common.TH (fromJust)

introduction
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadReader (Env t key) m
       , Palantype key
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
introduction = do

    Env {..} <- ask
    let Navigation {..} = envNavigation

    el "h1" $ text "Introduction"
    el "h2" $ text "Why Palantype"
    elClass "div" "paragraph"
        $ text
              "Palantype allows you to type with lightning speed. \
      \It's a stenographic system in the wider sense. \
      \The most widespread of these systems is simply called steno. \
      \Any steno-style system requires quite a bit of learning. \
      \Palantype has the advantage that it's more suitable for regular \
      \keyboards. There are limitations, though."

    el "h2" $ text "Requirements"

    el "h3" $ text "Hardware"

    elClass "div" "paragraph"
        $ text
              "You can get started with your regular keyboard. \
      \However, keyboards usually have limit of how many keys register \
      \at the same time. Long term, you will need a keyboard that supports \
      \N-Key roll-over (NKRO) to type chords of up to ten keys. \
      \In addition, a ortho-linear layout as well as very sensitive keys \
      \are preferable."

    elClass "div" "paragraph"
        $ text
              "You can play around with the keyboard above to see how much keys \
      \register at the same time with your hardware."

    elClass "p" "textAlign-center" $ elAttr "iframe"
        (    "width"  =: "640"
          <> "height" =: "480"
          <> "src"    =: "https://www.youtube.com/embed/za1qxU4jdfg"
        ) blank


    el "h3" $ text "Software"

    elClass "div" "paragraph" $ do
        text "All of this is made possible by the "
        elAttr "a" ("href" =: "http://www.openstenoproject.org/")
            $ text "Open Steno Project"
        text ". The software "
        elAttr "a" ("href" =: "http://www.openstenoproject.org/plover/")
            $ text "Plover"
        text
            " is part of the project and is all you need to get serious. \
      \As long as you practice here, you don't need Plover. \
      \Once you installed and configured Plover however, you can upload your \
      \Plover configuration here to practice with the same key map."
    elClass "div" "paragraph" $ do
        text "Be sure to check out additional information on "
        elAttr
                "a"
                ("href"
                =: "http://www.openstenoproject.org/palantype/tutorial/2016/08/21/learn-palantype.html"
                )
            $ text "learning Palantype"
        text " and the "
        elAttr
                "a"
                ("href"
                =: "http://www.openstenoproject.org/palantype/palantype/2016/08/21/palan-versus-steno.html"
                )
            $ text "differences between Palantype and Stenography"
        text "."

    elClass "blockquote" "warning" $ do
        el "strong" $ text "Current state of the project"
        el "br" blank
        case navLang of
            EN -> do
                text
                    "There exists a plover plugin for original English Palantype with a \
             \dictionary of about 22'000 entries–enough to play around. \
             \No basic commands are defined. \
             \As far as I know, there are no active users of the original \
             \Palantype system. In case I am mistaken, please reach out to me \
             \on the "
                elAttr "a" ("href" =: "https://discord.gg/spymr5aCr5")
                    $ text "Plover Discord Server"
                text
                    " in #palantype, @gurubm. Currently, only the exercises of Stage 1 \
             \are implemented, but adding further exercises for English Palantype is \
             \quite straightforward."
            DE ->
                text
                    "The steno dictionary for German contains \
             \1,945,685 words. Additional words are added regularly. \
             \Among the quality-of-life features are: command keys (Home, End, \
             \Insert, Delete, ...), special characters, fingerspelling, and \
             \plover commands for actual text formatting."

    let (rsStart, desc) = case navLang of
            EN -> ("START", "S-, T-, A, -R, and -T")
            DE -> ("DSAÜD", "D-, S-, A-, -Ü, -D")
        eChordSTART = void $ filter (== $fromJust (parseChordMaybe rsStart)) envEChord

    elClass "div" "start" $ do
        (btn, _) <- elClass' "button" "small" $ text "Get Started!"
        let eStart = leftmost [eChordSTART, domEvent Click btn]
        updateState
            $  eStart
            $> [ field @"stApp" . field @"stProgress" %~ Map.update (\_ -> Just $ $readLoc "stage_1-1") navLang
               , field @"stApp" . field @"stCleared" %~ Set.insert navCurrent
               , field @"stApp" . field @"stTOCShowStage1" .~ True
               ]
        setRoute $ eStart $> stageUrl navLang ($readLoc "stage_1-1")

    elClass "div" "paragraph" $ do
        text "Instead of clicking the button, try to input "
        el "code" $ text $ showt rsStart
        text
            $ " by pressing "
            <> desc
            <> " all at once. Take your time \
         \finding the next key while holding down. The chord is only registered \
         \once you release all the keys."
    pure envNavigation
