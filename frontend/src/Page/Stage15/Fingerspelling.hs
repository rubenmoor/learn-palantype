{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
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
import           Control.Monad                  ( unless )
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
                                                , RouteToUrl
                                                , SetRoute
                                                , routeLink
                                                )
import           Page.Common                    ( elCongraz
                                                , elNotImplemented
                                                , getStatsLocalAndRemote, chordStart
                                                )
import           Page.Common.Stopwatch          ( elStopwatch
                                                , mkStopwatch
                                                )
import           Palantype.Common               ( Chord
                                                , Palantype
                                                , StageSpecialGeneric(..)
                                                , SystemLang(SystemDE)
                                                , findStage
                                                , kiBackUp
                                                )
import qualified Palantype.Common.Indices      as KI
import qualified Palantype.Common.RawSteno     as Raw
import           Palantype.Common.TH            ( fromJust )
import qualified Palantype.DE                  as DE
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
                                                , updated
                                                )
import           Shared                         ( whenJust )
import           State                          ( Env(..)
                                                , Navigation(..)
                                                , State
                                                , stageUrl
                                                )
import           TextShow                       ( TextShow(showt) )

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
                elClass "span" "btnSteno blinking" $ do
                    text "Start "
                    el "code" $ text "SDAÜD"
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
                        elClass "span" "btnSteno blinking"
                            $  text
                            $  "↤ "
                            <> showt (KI.toRaw @key Palantype.Common.kiBackUp) -- U+21A4

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
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       , TriggerEvent t m
       )
    => m ()
fingerspelling = mdo
    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navSystemLang == Palantype.Common.SystemDE) elNotImplemented

    el "h1" $ text "Fingerspelling"

    el "h2" $ text "Palantype fingerspelling mode"

    elClass "div" "paragraph"
        $ text
              "Whenever a word does not exist in the installed dictionary, \
             \you won't be able to type it according to the rules presented here. \
             \Of course, the idea is that efficient steno chords exist for \
             \any conceivable situation, however ..."

    elClass "div" "paragraph" $ do
        text "There are"
        el "ul" $ do
            el "li"
                $ text "very rare words, not yet accounted for by the \
                       \algorithm,"
            el "li"
                $ text
                      "loanwords, partially accounted for by means of \
                           \explicit exceptions,"
            el "li" $ text "abbreviations and symbols, partially accounted for,"
            el "li" $ text "and diverging ways on how to write things."

    elClass "div" "paragraph" $ do
        text
            "This goes to say: If a word doesn't come out right, there is \
             \a troubleshooting procedure. Maybe there is a proper steno \
             \spelling, you just don't know it? In that case, you might \
             \want to look up the word within the dictionary, using Plover. \
             \If, however, there \
             \is no steno code for whatever you want to write, you have a \
             \general alternative that is called fingerspelling. \
             \I.e. there are steno codes for every single letter (and digit, \
             \special character, and command key) and, this way, it is possible to \
             \type sequentially, letter by letter, using steno."

    el "h3" $ text "Standard letters"

    elClass "div" "paragraph" $ do
        text
            "The Standard Letters are simply all those letters that are present \
             \on the US keyboard layout. They have special significance, because \
             \they commonly feature in shortcuts, like "
        el "code" $ text "CTRL+C"
        text " or "
        el "code" $ text "CTRL+-"
        text
            " (to zoom out in your internet browser). And indeed, the palantype \
             \fingerspelling mode accounts for that by providing modifier keys: "
        el "code" $ text "CTRL"
        text ", "
        el "code" $ text "WIN"
        text ", "
        el "code" $ text "ALT"
        text ", and "
        el "code" $ text "SHIFT"
        text ". Note that "
        el "code" $ text "SHIFT"
        text
            " is a secondary modifier and can be added on top of the other \
             \modifiers. This is why "
        el "code" $ text "SHIFT"
        text " has a finger apart from them."

    elClass "div" "paragraph" $ do
        text "Be aware that "
        el "code" $ text "WIN"
        text ", a key whose actual name is "
        el "code" $ text "SUPER"
        text ", appears strictly as modifier here. In case you want to tap the "
        el "code" $ text "WIN"
        text " key, e.g. to open the Start menu, the "
        el "code" $ text "WIN"
        text " key shows up among the keys in "
        let (iStage, iT, iS) =
              $fromJust $ Palantype.Common.findStage $ Palantype.Common.StageSpecial @DE.Key "Command Keys"
        routeLink (stageUrl @key iStage) $
          text $ "Ex. " <> showt iT <> "." <> showt iS
        text "."

    elClass "div" "paragraph" $ do
        text "Use "
        el "code" $ text "-L"
        text " in combination with the keys below, to start fingerspelling!"

    el "h4" $ text "Modifier keys"

    elClass "div" "patternTable" $ do
        elClass "div" "floatLeft" $ do
            elAttr "div" ("class" =: "orig") $ text "SHIFT"
            elAttr "code" ("class" =: "steno") $ text "-S"
        elClass "div" "floatLeft" $ do
            elAttr "div" ("class" =: "orig") $ text "CTRL"
            elAttr "code" ("class" =: "steno") $ text "s"
        elClass "div" "floatLeft" $ do
            elAttr "div" ("class" =: "orig") $ text "WIN"
            elAttr "code" ("class" =: "steno") $ text "D"
        elClass "div" "floatLeft" $ do
            elAttr "div" ("class" =: "orig") $ text "ALT"
            elAttr "code" ("class" =: "steno") $ text "n"
        elClass "br" "clearBoth" blank

    el "h4" $ text "Fingerspelling literals"

    elClass "div" "patternTable" $ do
        for_
            keysLetterUS
            \(c, steno) -> elClass "div" "floatLeft" $ do
                elAttr "div" ("class" =: "orig") $ text $ Text.singleton c
                elAttr "code" ("class" =: "steno") $ text steno
        elClass "br" "clearBoth" blank

    el "h3" $ text "Non-standard letters"

    elClass "div" "paragraph" $ do
        text
            "There is nothing special about the following letters. Only, \
             \they can be combined with SHIFT for their uppercase version \
             \and nothing else."

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

    el "h3" $ text "Beyond text transcription"

    elClass "div" "paragraph" $ do
        text "Fingerspelling is a powerfull feature. Together with "
        let (iStage, iT, iS) =
              $fromJust $ Palantype.Common.findStage @DE.Key $ Palantype.Common.StageSpecial "Command Keys"
        routeLink (stageUrl @key iStage) $
          text $ "Ex. " <> showt iT <> "." <> showt iS
        text
            " you can utilize any conventional key binding in steno mode \
             \without any additional configuration."
