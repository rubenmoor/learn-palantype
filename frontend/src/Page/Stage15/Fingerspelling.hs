{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Page.Stage15.Fingerspelling
    ( fingerspelling
    ) where

import           Common.Model                   ( Stats )
import           Common.Route                   ( FrontendRoute )
import           Control.Category               ( (.) )
import           Control.Lens                   ( (+~)
                                                , (.~)
                                                , (?~)
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader.Class     ( MonadReader
                                                , ask
                                                )
import           Data.Bool                      ( Bool
                                                , not
                                                )
import           Data.Eq                        ( Eq((==)) )
import           Data.Foldable                  ( Foldable(length)
                                                , for_
                                                )
import           Data.Function                  ( ($)
                                                , (&)
                                                )
import           Data.Functor                   ( (<$>)
                                                , (<&>)
                                                , Functor(fmap)
                                                )
import           Data.Generics.Product          ( field )
import           Data.Generics.Sum              ( _As )
import           Data.Int                       ( Int )
import           Data.List                      ( (!!)
                                                , filter
                                                , zip
                                                )
import           Data.Maybe                     ( Maybe(..)
                                                , isNothing
                                                )
import           Data.Ord                       ( Ord((>)) )
import           Data.Semigroup                 ( (<>)
                                                , Endo
                                                )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           Data.Tuple                     ( fst
                                                , snd
                                                )
import           GHC.Generics                   ( Generic )
import           GHC.Num                        ( Num((-)) )
import           Obelisk.Route.Frontend         ( R
                                                , SetRoute
                                                )
import           Page.Common                    ( elCongraz

                                                , getStatsLocalAndRemote, chordStart
                                                )
import           Page.Common.Stopwatch          ( elStopwatch
                                                , mkStopwatch
                                                )
import           Palantype.Common               ( Chord
                                                , Palantype



                                                , kiBackUp
                                                )
import qualified Palantype.Common.Indices      as KI
import qualified Palantype.Common.RawSteno     as Raw
import           Palantype.DE.FingerSpelling    ( dictLiterals
                                                , keysLetterOther
                                                , keysLetterUS
                                                )
import           Reflex.Dom                     ( (=:)
                                                , DomBuilder
                                                , Dynamic
                                                , Event
                                                , EventWriter
                                                , MonadHold
                                                , PerformEvent
                                                , Performable
                                                , PostBuild
                                                , Prerender
                                                , TriggerEvent
                                                , blank
                                                , current
                                                , dyn_
                                                , el
                                                , elAttr
                                                , elClass
                                                , foldDyn
                                                , gate
                                                , holdUniqDyn
                                                , text
                                                , updated, splitE
                                                )
import           Shared                         ( whenJust )
import           State                          ( Env(..)

                                                , State

                                                )
import           TextShow                       ( TextShow(showt) )
import CMS (elCMS, elCMSContent)
import Witherable (Filterable(mapMaybe))
import Data.Bifunctor (Bifunctor(first))

data StateLiterals k
    = StatePause Int
    | StateRun (Run k)
    deriving (Generic)

data Run k = Run
    { stCounter  :: Int
    , stMMistake :: Maybe (Int, Palantype.Common.Chord k)
    , stNMistakes :: Int
    } deriving (Generic)

{-|
pass through all the letters of the steno alphabet one by one
-}
taskLiterals
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadIO (Performable m)
       , MonadReader (Env t key) m
       , Palantype.Common.Palantype key
       , PerformEvent t m
       , PostBuild t m
       , Prerender t m
       , TriggerEvent t m
       )
    => Dynamic t [(Bool, (Maybe Text, Stats))]
    -> Event t (Palantype.Common.Chord key)
    -> m (Event t Stats)
taskLiterals dynStats evChord = do
    --Env {..} <- ask
    let
        --Navigation {..} = envNavigation
        len             = length dictLiterals

        step :: Palantype.Common.Chord key -> StateLiterals key -> StateLiterals key
        step c st = case st of
            StatePause _ ->
                if c == chordStart
                    then stepStart
                    else st
            StateRun Run {..} ->
                let currentLetter = KI.toRaw @key $ fst $ dictLiterals !! stCounter
                in
                    case stMMistake of

                        -- mistake mode ...
                        -- ... back up
                        Just _ | Raw.fromChord c == KI.toRaw @key Palantype.Common.kiBackUp ->
                            st & _As @"StateRun" . field @"stMMistake" .~ Nothing
                        -- ... or do nothing
                        Just _ -> st

                        -- correct
                        Nothing | Raw.fromChord c == currentLetter ->
                            if stCounter == len - 1
                                then StatePause stNMistakes
                                else st & _As @"StateRun" . field @"stCounter" +~ 1

                        -- mistake
                        Nothing ->
                            st
                                &  _As @"StateRun"
                                .  field @"stMMistake"
                                ?~ (stCounter, c)
                                &  _As @"StateRun"
                                .  field @"stNMistakes"
                                +~ 1

        stepStart = StateRun Run { stCounter   = 0
                                 , stMMistake  = Nothing
                                 , stNMistakes = 0
                                 }
        stateInitial = StatePause 0

    dynLiterals <- foldDyn step stateInitial evChord

    evStartStop <- fmap updated $ holdUniqDyn $ dynLiterals <&> \case
        StatePause nMistakes -> nMistakes
        StateRun   _         -> -1

    dynStopwatch <- mkStopwatch evStartStop

    elClass "div" "paragraph" $ do
        dyn_ $ dynLiterals <&> \case
            StatePause _ -> el "div" $ do
                text "Type "
                elClass "span" "steno-action" $ do
                    text "Start "
                    el "code" $ text $ showt $ chordStart @key
                text " to begin the exercise."
            StateRun Run {..} -> do
                elClass "div" "exerciseField multiline"
                    $ el "code"
                    $ for_ (zip [0 :: Int ..] dictLiterals)
                    $ \(i, (_, lit)) ->
                          let
                              cls = case stMMistake of
                                  Just (j, _) -> if i == j then "bgRed" else ""
                                  Nothing ->
                                      if stCounter > i then "bgGreen" else ""
                          in  elClass "span" cls $ text lit

                whenJust stMMistake $ \(_, w) ->
                    elClass "div" "red small paragraph" $ do
                        text $ "You typed " <> showt w <> " "
                        elClass "span" "steno-navigation"
                            $  text
                            $  "↤ "
                            <> showt (KI.toRaw @key kiBackUp) -- U+21A4

                text $ showt stCounter <> " / " <> showt len

        elStopwatch dynStats dynStopwatch len

fingerspelling
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadIO (Performable m)
       , MonadReader (Env t key) m
       , Palantype.Common.Palantype key
       , PerformEvent t m
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       , TriggerEvent t m
       )
    => m ()
fingerspelling = mdo
    Env {..} <- ask

    ((evPart1, evPart2), evPart3) <- elCMS 3 <&> first splitE . splitE . mapMaybe \case
      [p1, p2, p3] -> Just ((p1, p2), p3)
      _            -> Nothing

    elCMSContent evPart1

    elClass "div" "patternTable" $ do
        for_ keysLetterUS \(c, steno) -> elClass "div" "floatLeft" $ do
            elAttr "div" ("class" =: "orig") $ text $ Text.singleton c
            elAttr "code" ("class" =: "steno") $ text steno
        elClass "br" "clearBoth" blank

    elCMSContent evPart2

    elClass "div" "patternTable" $ do
        for_
            keysLetterOther
            \(c, steno) -> elClass "div" "floatLeft" $ do
                elAttr "div" ("class" =: "orig") $ text $ Text.singleton c
                elAttr "code" ("class" =: "steno") $ text steno
        elClass "br" "clearBoth" blank

    dynStatsAll <- getStatsLocalAndRemote evDone
    evDone <- taskLiterals dynStatsAll $ gate (not <$> current dynDone) envEChord
    let dynStatsPersonal = fmap snd . filter (isNothing . fst) . fmap snd <$> dynStatsAll
    dynDone <- elCongraz (Just <$> evDone) dynStatsPersonal envNavigation

    elCMSContent evPart3
