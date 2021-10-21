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

import           Common.Route           (FrontendRoute (..))
import           Control.Applicative    (Applicative (pure))
import           Control.Lens           ((%~), (.~))
import           Control.Monad.Reader   (MonadReader (ask))
import           Data.Bool              (Bool (..))
import           Data.Eq                (Eq ((==)))
import           Data.Function          (($))
import           Data.Functor           (void, ($>))
import           Data.Generics.Product  (field)
import           Data.Monoid            (Monoid (mconcat))
import           Data.Semigroup         (Endo (..))
import qualified Data.Set               as Set
import           Data.Witherable        (Filterable (filter))
import           Obelisk.Route.Frontend (pattern (:/), R, SetRoute (setRoute))
import           Reflex.Dom             (DomBuilder, EventName (Click),
                                         EventWriter, HasDomEvent (domEvent),
                                         el, elAttr, elClass, elClass',
                                         leftmost, text, (=:))
import           State                  (State, Env (..),
                                         Navigation (..), Stage (..),
                                         updateState)
import Palantype.Common.RawSteno (parseChordLenient)
import qualified Data.Map as Map
import Data.Maybe (Maybe(Just))
import Palantype.Common (Palantype)

introduction
  :: forall key t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t (Endo State) m
  , MonadReader (Env t key) m
  , Palantype key
  , SetRoute t (R FrontendRoute) m
  )
  => m Navigation
introduction = do

  Env{..} <- ask

  el "h1" $ text "Introduction"
  el "h2" $ text "Why Palantype"
  elClass "div" "paragraph" $
    text
      "Palantype allows you to type with lightning speed. \
      \It's a stenographic system in the wider sense. \
      \The most widespread of these systems is simply called steno. \
      \Any steno-style system requires quite a bit of learning. \
      \Palantype has the advantage that it's more suitable for regular \
      \keyboards. There are limitations, though."
  el "h2" $ text "Requirements"
  el "h3" $ text "Hardware"
  elClass "div" "paragraph" $
    text
      "You can get started with your regular keyboard. \
      \However, keyboards usually have limit of how many keys register \
      \at the same time. Long term, you will need a keyboard that supports \
      \N-Key roll-over (NKR) to type chords of up to ten keys. \
      \In addition, a ortholinear layout as well as very sensitive keys \
      \are preferable."
  elClass "div" "paragraph" $
    text
      "You can play around with the keyboard above to see how much keys \
      \register at the same time with your hardware."

  el "h3" $ text "Software"
  elClass "div" "paragraph" $ do
    text "All of this is made possible by the "
    elAttr "a" ("href" =: "http://www.openstenoproject.org/") $ text "Open Steno Project"
    text ". The software "
    elAttr "a" ("href" =: "http://www.openstenoproject.org/plover/") $ text "Plover"
    text
      " is part of the project and is all you need to get serious. \
      \As long as you practice here, you don't need Plover. \
      \Once you installed and configured Plover however, you can upload your \
      \Plover configuration here to practice with the same key map."
  elClass "div" "paragraph" $ do
    text "Be sure to check out additional information on "
    elAttr "a" ("href" =: "http://www.openstenoproject.org/palantype/tutorial/2016/08/21/learn-palantype.html") $ text "learning Palantype"
    text " and the "
    elAttr "a" ("href" =: "http://www.openstenoproject.org/palantype/palantype/2016/08/21/palan-versus-steno.html") $ text "differences between Palantype and Stenography"
    text "."

  -- TODO lang
  let eChordSTART = void $ filter (== parseChordLenient "SDA~T") envEChord
      Navigation{..} = envNavigation

  elClass "div" "start" $ do
    (btn, _) <- elClass' "button" "start" $ text "Get Started!"
    let eStart = leftmost [eChordSTART, domEvent Click btn]
    updateState $ eStart $>
      [ field @"stProgress" %~ Map.update (\_ -> Just Stage1_1) navLang
      , field @"stCleared" %~ Set.insert navCurrent
      , field @"stTOCShowStage1" .~ True
      ]
    setRoute $ eStart $> FrontendRoute_Main :/ ()

  elClass "div" "paragraph" $ do
    text "Instead of clicking the button, try to input "
    el "code" $ text "START"
    text " by pressing S-, T-, A, -R, and -T all at once. Take your time \
         \finding the next key while holding down. The chord is only registered \
         \once you release all the keys."
  pure envNavigation
