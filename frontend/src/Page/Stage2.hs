{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Page.Stage2 where

import           Common.Api                     ( Lang(..) )
import           Common.Route                   ( FrontendRoute(..) )
import           Control.Applicative            ( (<$>)
                                                , Applicative(pure)
                                                )
import           Control.Category               ( Category((.), id) )
import           Control.Lens                   ( (%~)
                                                , (<&>)
                                                )
import           Control.Monad                  ( when )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.Reader           ( MonadReader(ask) )
import           Data.Bool                      ( Bool(..) )
import           Data.Eq                        ( Eq((==)) )
import           Data.Foldable                  ( Foldable(length)
                                                , for_
                                                , traverse_
                                                )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( ($>)
                                                , void
                                                )
import           Data.Generics.Product          ( field )
import           Data.Int                       ( Int )
import           Data.List                      ( (!!)
                                                , zip
                                                )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( Maybe(..) )
import           Data.Ord                       ( Ord((>)) )
import           Data.Semigroup                 ( Endo
                                                , Semigroup((<>))
                                                )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Witherable                ( Filterable(catMaybes, filter)
                                                )
import           GHC.Num                        ( Num((+), (-)) )
import           Obelisk.Route.Frontend         ( pattern (:/)
                                                , R
                                                , SetRoute(setRoute)
                                                )
import           Page.Common                    ( elCongraz
                                                , getChordBack
                                                , getChordCon
                                                )
import           Palantype.Common               ( Chord
                                                , Palantype
                                                )
import           Palantype.Common.RawSteno      ( RawSteno(..)
                                                , parseChordLenient
                                                , parseStenoLenient
                                                )
import           Reflex.Dom                     ( (=:)
                                                , DomBuilder
                                                , EventName(Click)
                                                , EventWriter
                                                , HasDomEvent(domEvent)
                                                , MonadHold(holdDyn)
                                                , PostBuild
                                                , Reflex(Event, updated)
                                                , blank
                                                , dyn_
                                                , el
                                                , elAttr
                                                , elClass
                                                , elClass'
                                                , elDynClass
                                                , foldDyn
                                                , leftmost
                                                , text
                                                , widgetHold_
                                                )
import           Shared                         ( whenJust )
import           State                          ( Env(..)
                                                , Navigation(..)
                                                , Stage(Stage1_1)
                                                , State(..)
                                                , stageUrl
                                                , updateState
                                                )
import           Text.Show                      ( Show(show) )
import           TextShow                       ( TextShow(showt) )

backUp :: Lang -> RawSteno
backUp = \case
    EN -> "ULFTS"
    DE -> "ILKSD"

exercise1
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise1 = do

    Env {..} <- ask
    let Navigation {..} = envNavigation


    el "h1" $ text "Stage 2"
    el "h2" $ text "Syllables and chords"
    el "h3" $ text "Exercise 1"
    elClass "div" "paragraph" $ do
        text
            "You probably have noticed that on the virtual keyboard some keys \
         \are highlighted blue. These keys are called home row. \
         \The idea is that there is a resting position for your hands, where \
         \every finger is placed on one key of home row."
    elClass "div" "paragraph" $ do
        text
            "From this resting position your fingers can find any key, without \
          \any need to look down on your keyboard, so your eyes can stay on \
          \the screen all the time."
    elClass "div" "paragraph" $ do
        text
            "In case you have been looking on your fingers during the exercises, \
         \it's probably a good idea to get used to home row now. \
         \Simply repeat Stage 1 until you can do all the exercises without \
         \looking down. Just orient yourself once in the beginning!"

    let (rsCon , eChordCon ) = getChordCon navLang envEChord
        (rsBack, eChordBack) = getChordBack navLang envEChord

    elABack <- elClass "div" "paragraph" $ do
        text "Type "
        el "code" $ text $ showt rsBack
        text " to "
        (e, _) <- elClass' "a" "normalLink" $ text "go back to Exercise 1.1"
        text " to practice home row."
        pure e

    let eBack = leftmost [eChordBack, domEvent Click elABack]

    setRoute $ eBack $> stageUrl navLang Stage1_1
    updateState
        $  eBack
        $> [field @"stProgress" %~ Map.update (\_ -> Just Stage1_1) navLang]

    whenJust navMNext $ \nxt -> do
        (elACont, _) <- elClass "div" "anthrazit" $ do
            text "Type "
            el "code" $ text $ showt rsCon
            text " to continue to "
            elClass' "a" "normalLink" $ text $ Text.pack $ show nxt
        let eContinue = leftmost [eChordCon, domEvent Click elACont]
        updateState
            $  eContinue
            $> [ field @"stProgress"
                   %~ Map.update
                          (\s -> if nxt > s then Just nxt else Just s)
                          navLang
               , field @"stCleared" %~ Set.insert navCurrent
               ]
        setRoute $ eContinue $> FrontendRoute_Main :/ ()

    pure envNavigation

exercise2
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise2 = do

    Env {..} <- ask
    let Navigation {..} = envNavigation

    el "h1" $ text "Stage 2"
    el "h2" $ text "Syllables and chords"
    el "h3" $ text "Exercise 2"

    eDone <- case navLang of
        EN -> do
            elClass "div" "paragraph" $ do
                text
                    "We can begin with actually typing sentences now. \
             \How about this one sentences I found in the "
                elAttr
                        "a"
                        ("href"
                        =: "http://www.openstenoproject.org/palantype/tutorial/2016/08/21/learn-palantype.html"
                        )
                    $ text "Palantype Tutorial"
                text " of the Open Steno Project:"

            let raw = "TH CFIC P+RAUN FOCS +YUMPS OEFR TH LE^/S+I T+OC+ ^"
                lsWord =
                    [ "The"
                    , "quick"
                    , "brown"
                    , "fox"
                    , "jumps"
                    , "over"
                    , "the"
                    , "lazy"
                    , ""
                    , "dog"
                    , "."
                    ]

            eDone <- walkWords lsWord raw

            elClass "div" "paragraph" $ do
                text
                    "Each word is one chord, except the word \"lazy\". You will \
             \have to strike "
                el "code" $ text "LE^"
                text " and "
                el "code" $ text "S+I"
                text " separately. For this reason, the steno code for \"lazy\" \
                     \is denoted "
                el "code" $ text "LE^/S+I"
                text ", with a /."

            pure eDone
        DE -> do
            elClass "div" "paragraph"
                $ text
                      "We can begin with actually typing sentences now. \
             \How about this quote from Goethe:"

            -- TODO: punctuation
            let raw = "MID DEM WISn WÄKSD DEÜ SFEI/FEL"
                txt = "Mit dem Wissen wächst der Zweifel"

            eDone <- walkWords (Text.words txt) raw

            elClass "div" "paragraph" $ do
                text
                    "Each word is one chord, except the word «Zweifel». You will \
             \have to strike "
                el "code" $ text "SFEI"
                text " and "
                el "code" $ text "FEL"
                text " separately. This is what the /-symbol means."
            pure eDone

    elClass "div" "paragraph" $ do
        text "Let me introduce yet another useful chord: "
        el "code" $ text $ showt $ backUp navLang
        text
            ". It is the homerow of your right hand and deletes your last \
         \input. Now you can correct your mistakes!"

    elCongraz eDone envNavigation

    pure envNavigation

data WalkState = WalkState
    { wsCounter  :: Int
    , wsMMistake :: Maybe Int
    , wsDone     :: Maybe Bool
    }

walkWords
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       )
    => [Text]
    -> RawSteno
    -> m (Event t ())
walkWords words raw = do
    Env {..} <- ask
    let Navigation {..} = envNavigation

    let cBackUp = parseChordLenient $ backUp navLang
        chords  = parseStenoLenient raw
        len     = length chords

        step :: Chord key -> WalkState -> WalkState
        step chord ws@WalkState {..} = case (wsMMistake, wsDone) of
            (_, Just True) -> ws { wsDone = Just False, wsCounter = 0 } -- reset after done
            _ | wsCounter == len - 1 ->
                ws { wsDone = Just True, wsCounter = wsCounter + 1 } -- done
            _ | chord == cBackUp ->
                ws { wsMMistake = Nothing, wsCounter = wsCounter - 1 }  -- undo stroke
            (Just _, _) -> ws   -- halt while mistake
            (_, _) | chords !! wsCounter == chord ->
                ws { wsDone = Nothing, wsCounter = wsCounter + 1 } -- correct
            (_, _) -> ws { wsDone = Nothing, wsMMistake = Just wsCounter } -- mistake

        stepInitial =
            WalkState { wsCounter = 0, wsMMistake = Nothing, wsDone = Nothing }

    dynWalk <- foldDyn step stepInitial envEChord
    let eDone = catMaybes $ wsDone <$> updated dynWalk

    el "blockquote" $ el "table" $ do
        el "tr" $ traverse_ (el "td" . text) words
        el "tr" $ do
            for_ (zip [0 :: Int ..] chords) $ \(i, c) -> do
                let dynCls = dynWalk <&> \WalkState {..} -> case wsMMistake of
                        Just j  -> if i == j then "bgRed" else ""
                        Nothing -> if wsCounter > i then "bgGreen" else ""
                el "td" $ elDynClass "pre" dynCls $ el "code" $ text $ showt c

            el "td" $ do
                let eMistake = wsMMistake <$> updated dynWalk
                widgetHold_ blank $ eMistake <&> \case
                    Just _ -> elClass "code" "blinking" $ text $ " " <> showt
                        (backUp navLang)
                    Nothing -> blank

    dynDone <- holdDyn False eDone
    dyn_ $ dynDone <&> \bDone ->
        when bDone $ elClass "div" "small anthrazit" $ text
            "Cleared. Press any key to start over."

    pure $ void $ filter id eDone
