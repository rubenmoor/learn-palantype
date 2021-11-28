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

import           Client                         ( getDictTop2k
                                                , postRender

                                                )
import           Common.Api                     ( Lang(..) )
import           Common.Route                   ( FrontendRoute(..) )
import           Control.Applicative            ( (<$>)
                                                , Applicative(pure)
                                                )
import           Control.Category               ( Category((.), id) )
import           Control.Lens                   ( (%~)
                                                , (<&>)
                                                )
import           Control.Monad                  ( (=<<)
                                                , when
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Random           ( evalRand )
import           Control.Monad.Reader           ( MonadReader(ask)
                                                , asks
                                                )
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
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.Int                       ( Int )
import           Data.List                      (intersperse, (++), elem,  (!!)
                                                , zip
                                                )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( Maybe(..)
                                                , fromMaybe
                                                )
import           Data.Ord                       ( Ord((<), (>), max) )
import           Data.Semigroup                 ( Endo
                                                , Semigroup((<>))
                                                )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Witherable                ( Filterable
                                                    ( catMaybes
                                                    , filter
                                                    , mapMaybe
                                                    )
                                                )
import           GHC.Num                        ( Num((+), (-)) )
import           Obelisk.Route.Frontend         ( pattern (:/)
                                                , R
                                                , SetRoute(setRoute)
                                                )
import           Page.Common                    (backUp, elNotImplemented,  elCongraz
                                                , getChordBack
                                                , getChordCon
                                                )
import           Palantype.Common               ( Chord(..)
                                                , Palantype
                                                )
import           Palantype.Common.RawSteno      ( RawSteno(..) )
import qualified Palantype.Common.RawSteno     as Raw
import           Reflex.Dom                     ( (=:)
                                                , DomBuilder
                                                , EventName(Click)
                                                , EventWriter
                                                , HasDomEvent(domEvent)
                                                , MonadHold(holdDyn)
                                                , PerformEvent(performEvent)
                                                , PostBuild(getPostBuild)
                                                , Prerender
                                                , Reflex
                                                    ( Event
                                                    , never
                                                    , updated
                                                    )

                                                , blank
                                                , delay
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
                                                , zipDyn
                                                )
import           Servant.Common.Req             ( ReqResult(..)
                                                , reqSuccess
                                                )
import           Shared                         ( dynSimple
                                                , iFa
                                                , whenJust

                                                )
import           State                          ( Env(..)
                                                , Navigation(..)
                                                , Stage(Stage1_1)
                                                , State(..)
                                                , stageUrl
                                                , updateState
                                                )
import           System.Random                  ( newStdGen )
import           System.Random.Shuffle          ( shuffleM )
import           Text.Show                      ( Show(show) )
import           TextShow                       ( TextShow(showt) )
import Control.Monad (unless)
import Data.Sequence ((|>), Seq)
import Safe (initMay)

exercise1
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadReader (Env t key) m
       , Palantype key
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise1 = do

    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang `elem` [DE, EN]) elNotImplemented

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

-- Ex 2.2

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

    let chords  = Raw.parseStenoLenient raw
        len     = length chords

        step :: Chord key -> WalkState -> WalkState
        step chord ws@WalkState {..} = case (wsMMistake, wsDone) of
            (_, Just True) -> ws { wsDone = Just False, wsCounter = 0 } -- reset after done
            _ | wsCounter == len - 1 ->
                ws { wsDone = Just True, wsCounter = wsCounter + 1 } -- done
            _ | Raw.fromChord chord == backUp navLang ->
                ws { wsMMistake = Nothing, wsCounter = max 0 $ wsCounter - 1 }  -- undo stroke
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
    unless (navLang `elem` [DE, EN]) elNotImplemented

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
                text
                    " separately. For this reason, the steno code for \"lazy\" \
                     \is typically denoted "
                el "code" $ text "LE^/S+I"
                text ", with a /."

            elClass "div" "paragraph" $ do
                text
                    "You wonder why the steno code looks so weird? \
                   \Some words are almost beyond recognition. \
                   \No worries, we'll get to that."

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
                text
                    " separately. For this reason, the steno code for «Zweifel» \
                     \is typically denoted "
                el "code" $ text "SFEI/FEL"
                text ", with a /."

            elClass "div" "paragraph" $ do
                text
                    "You wonder why the steno code looks so weird? \
                   \Some words are almost beyond recognition. \
                   \No worries, we'll get to that."

            pure eDone

    elClass "div" "paragraph" $ do
        text "Let me introduce yet another useful chord: "
        el "code" $ text $ showt $ backUp navLang
        text
            ". It is the homerow of your right hand and deletes your last \
         \input. Now you can correct your mistakes!"

    elCongraz eDone envNavigation

    pure envNavigation

-- Ex 2.3

data StenoSingletonsState = StenoSingletonsState
    { ssstCounter   :: Int
    , ssstMMistake  :: Maybe StateMistake
    , ssstDone      :: Maybe Bool
    , ssstWords     :: [Text]
    , ssstNMistakes :: Int
    }

data StateMistake
  = MistakeOne RawSteno
  | MistakeTwo RawSteno [RawSteno]

taskSingletons
    :: forall key js t (m :: * -> *)
     . ( DomBuilder t m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender js t m
       )
    => [Text]
    -> Event t (HashMap RawSteno Text, HashMap Text [RawSteno])
    -> m (Event t ())
taskSingletons words eMaps = do

    eChord  <- asks envEChord

    eStdGen <- postRender $ do
        ePb <- getPostBuild
        performEvent $ ePb $> liftIO newStdGen

    dynMStdGen <- holdDyn Nothing $ Just <$> eStdGen
    dynMMaps   <- holdDyn Nothing $ Just <$> eMaps

    dynSimple $ zipDyn dynMStdGen dynMMaps <&> \case
        (Nothing    , _      ) -> pure never
        (_          , Nothing) -> pure never
        (Just stdGen, Just (mapStenoWord, mapWordStenos)) -> do
            let len = length words

                step :: Chord key -> StenoSingletonsState -> StenoSingletonsState
                step c ls@StenoSingletonsState {..} =
                    case (Raw.fromChord c, ssstDone) of

                        -- reset after done
                        (_, Just True) ->
                            let words' = evalRand (shuffleM ssstWords) stdGen
                            in  ls { ssstDone    = Just False
                                   , ssstCounter = 0
                                   , ssstWords   = words'
                                   }

                        -- done
                        _ | ssstCounter == len - 1 -> ls
                            { ssstDone    = Just True
                            , ssstCounter = ssstCounter + 1
                            }

                        (raw, _) ->
                            let word = ssstWords !! ssstCounter
                            in
                                if fromMaybe
                                        ""
                                        (HashMap.lookup raw mapStenoWord)
                                    == word
                                   -- correct
                                then
                                    ls { ssstDone     = Nothing
                                       , ssstCounter  = ssstCounter + 1
                                       , ssstMMistake = Nothing
                                       }
                                else
                                    case ssstMMistake of
                                      -- first mistake
                                        Nothing -> ls
                                            { ssstDone     = Nothing
                                            , ssstMMistake = Just
                                                                $ MistakeOne raw
                                            }

                                        -- second mistake
                                        Just (MistakeOne _) ->
                                            let
                                                corrects =
                                                    fromMaybe []
                                                        $ HashMap.lookup
                                                              word
                                                              mapWordStenos
                                            in
                                                ls
                                                    { ssstDone     = Nothing
                                                    , ssstMMistake =
                                                        Just $ MistakeTwo
                                                            raw
                                                            corrects
                                                    }

                                        -- third mistake and so forth
                                        Just (MistakeTwo _ _) -> ls

                stepInitial = StenoSingletonsState
                    { ssstCounter   = 0
                    , ssstMMistake  = Nothing
                    , ssstDone      = Nothing
                    , ssstWords     = evalRand (shuffleM words) stdGen
                    , ssstNMistakes = 0
                    }

            dynStenoWords <- foldDyn step stepInitial eChord

            let eDone = catMaybes $ ssstDone <$> updated dynStenoWords

            elClass "div" "taskSingletons" $ do
                el "span" $ dyn_ $ dynStenoWords <&> \StenoSingletonsState {..} -> do
                    when (ssstCounter < len)
                        $  el "pre"
                        $  el "code"
                        $  text
                        $  ssstWords
                        !! ssstCounter

                let eMMistake = ssstMMistake <$> updated dynStenoWords
                widgetHold_ blank $ eMMistake <&> \case
                    Just (MistakeOne raw) -> do
                        elClass "code" "red small" $ text $ showt raw
                        elClass "span" "small" $ text " try again!"
                    Just (MistakeTwo raw corrects) -> do
                        elClass "code" "red small" $ text $ showt raw
                        elClass "span" "small" $ text $ if length corrects == 1
                            then " try this: "
                            else " try one of these: "
                        for_ corrects $ \correct ->
                            elClass "code" "small" $ text $ showt correct
                    Nothing -> blank

            let dynCounter = ssstCounter <$> dynStenoWords
            dyn_ $ dynCounter <&> \c -> elClass "div" "paragraph" $ do
                el "strong" $ text $ showt c
                text " / "
                text $ showt len

            dynDone <- holdDyn False eDone
            dyn_ $ dynDone <&> \bDone ->
                when bDone $ elClass "div" "small anthrazit" $ text
                    "Cleared. Press any key to start over."

            pure $ void $ filter id eDone

exercise3
    :: forall js key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender js t m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise3 = do

    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang == DE) elNotImplemented

    el "h1" $ text "Stage 2"
    el "h2" $ text "Syllables and chords"
    el "h3" $ text "Exercise 3"

    elClass "div" "paragraph"
        $ text
              "During the following exercises, you will learn to type words, \
           \starting with the most straightfoward ones. \
           \You will be able to guess the correct chord without problem."

    elClass "div" "paragraph"
        $ text
              "There are rules that will become progressively more complex \
           \in the course of this tutorial. We start simple:"

    el "h4" $ text "Rule 1: Steno key order"

    elClass "div" "paragraph"
        $ text
              "Every chord consists of up to ten keys \
           \pressed at once. \
           \Within one chord, the order in which you press down keys does not \
           \matter. \
           \Instead, all keys of one chord will always be interpreted \
           \in their proper order."

    elClass "div" "paragraph" $ do
        text "For example, the steno keys "
        el "code" $ text "BUʃ"
        text
            " can only appear in exactly that order and always mean «Busch». \
           \The word «Schub» has to be typed using different keys \
           \, and indeed here it is: "
        el "code" $ text "SJUP"
        text "."


    el "h4" $ text "Rule 2: Word part structure"

    elClass "div" "paragraph"
        $ text
              "One chord either makes a word or a word part. \
           \In general, a part consists of an onset, a nucleus, \
           \and a coda. The onset comprises the consonants in the beginning \
           \and can be missing. The nucleus comprises the vowels that follow \
           \and the coda finally comprises the consonants in the end."

    elClass "div" "paragraph"
        $ text
              "For a word part structured that way, you will use the fingers \
      \of your left hand for the consonants of the onset and the fingers \
      \of your right hand for the consonants of the coda. \
      \For the nucleus you have your thumb."

    elClass "div" "paragraph" $ do
        text "The example «Busch» shows, how "
        el "em" $ text "b"
        text " and "
        el "em" $ text "sch"
        text
            " have different steno keys, depending on where they appear. \
           \In the onset "
        el "em" $ text "b"
        text " is simply "
        el "code" $ text "B"
        text ", whereas in the coda, "
        el "em" $ text "b"
        text " is "
        el "code" $ text "P"
        text "."

    elClass "div" "paragraph" $ text "Type the following words as they appear!"

    ePb     <- postRender $ delay 0.1 =<< getPostBuild
    eResult <- postRender (getDictTop2k ePb)
    let eSuccess = mapMaybe reqSuccess eResult
    dynResult <- holdDyn Nothing $ Just <$> eResult

    dyn_ $ dynResult <&> \case
        Nothing -> elClass "div" "paragraph" $ do
            iFa "fas fa-spinner fa-spin"
            text " Loading ..."
        Just (ResponseSuccess _ _ _) -> blank
        Just _                       -> elClass "div" "paragraph small red"
            $ text "Could not load resource: top2k"

    eDone <- taskSingletons words2_3 eSuccess

    elCongraz eDone envNavigation
    pure envNavigation

words2_3 :: [Text]
words2_3 =
    [ "und"
    , "in"
    , "das"
    , "den"
    , "im"
    , "auf"
    , "es"
    , "ein"
    , "dem"
    , "des"
    , "am"
    , "an"
    , "als"
    , "bei"
    , "aus"
    , "um"
    , "so"
    , "man"
    , "bis"
    , "sein"
    , "was"
    , "nun"
    , "beim"
    , "da"
    , "drei"
    , "uns"
    , "rund"
    , "weil"
    , "mal"
    , "ins"
    , "ja"
    , "wo"
    , "Land"
    , "fünf"
    , "frau"
    , "Mai"
    , "du"
    , "je"
    , "neu"
    , "Haus"
    , "hin"
    , "bald"
    , "Bild"
    , "mein"
    , "Bund"
    , "Hand"
    , "raum"
    , "neun"
    , "frei"
    , "Bad"
    , "los"
    , "fand"
    , "AfD"
    , "elf"
    , "Grad"
    , "Mensch"
    , "Brand"
    , "nein"
    , "aufs"
    , "eins"
    , "Bau"
    ]

-- Ex 2.4

data StenoWordsState = StenoWordsState
    { swsCounter   :: Int
    , swsDone      :: Maybe Bool
    , swsChords    :: [RawSteno]
    , swsWords     :: [Text]
    , swsNMistakes :: Int
    }

taskWords
    :: forall key js t (m :: * -> *)
     . ( DomBuilder t m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender js t m
       )
    => [Text]
    -> Event t (HashMap RawSteno Text, HashMap Text [RawSteno])
    -> m (Event t ())
taskWords words eMaps = do

  Env {..} <- ask
  let Navigation {..} = envNavigation

  eStdGen <- postRender $ do
      ePb <- getPostBuild
      performEvent $ ePb $> liftIO newStdGen

  dynMStdGen <- holdDyn Nothing $ Just <$> eStdGen
  dynMMaps   <- holdDyn Nothing $ Just <$> eMaps

  dynSimple $ zipDyn dynMStdGen dynMMaps <&> \case
    (Nothing    , _      ) -> pure never
    (_          , Nothing) -> pure never
    (Just stdGen, Just (mapStenoWord, mapWordStenos)) -> do
      let len = length words

          step :: Chord key -> StenoWordsState -> StenoWordsState
          step c ls@StenoWordsState {..} =
            -- let raw = Text.intercalate "/" $ showt <$> swsChords ++ [Raw.fromChord c]
            case (Raw.fromChord c, swsDone) of

              -- reset after done
              (_, Just True) ->
                let words' = evalRand (shuffleM swsWords) stdGen
                in  ls { swsDone    = Just False
                       , swsCounter = 0
                       , swsWords   = words'
                       }

              -- done
              _ | swsCounter == len - 1 -> ls
                  { swsDone    = Just True
                  , swsCounter = swsCounter + 1
                  }

              -- undo last input
              (r, _) | r == backUp navLang -> ls
                  { swsChords = fromMaybe [] $ initMay swsChords
                  , swsNMistakes = swsNMistakes + 1
                  }

              (raw, _) ->
                let word = swsWords !! swsCounter
                    rawWord = Raw.unparts $ swsChords ++ [raw]
                in
                    if fromMaybe
                            ""
                            (HashMap.lookup rawWord mapStenoWord)
                        == word
                       -- correct
                    then
                        ls { swsDone     = Nothing
                           , swsCounter  = swsCounter + 1
                           }
                    else
                        ls { swsDone     = Nothing
                           , swsChords   = swsChords ++ [raw]
                           }

          stepInitial = StenoWordsState
              { swsCounter   = 0
              , swsChords = []
              , swsDone      = Nothing
              , swsWords     = evalRand (shuffleM words) stdGen
              , swsNMistakes = 0
              }

      dynStenoWords <- foldDyn step stepInitial envEChord

      let eDone = catMaybes $ swsDone <$> updated dynStenoWords

      elClass "div" "taskWords" $ do
        dyn_ $ dynStenoWords <&> \StenoWordsState {..} -> do
          el "span" $
              when (swsCounter < len)
                  $  el "pre"
                  $  el "code"
                  $  text
                  $  swsWords !! swsCounter

          el "span" $ for_ (intersperse "/" $ showt <$> swsChords) $ \str ->
            el "code" $ text str

      let dynCounter = swsCounter <$> dynStenoWords
      dyn_ $ dynCounter <&> \c -> elClass "div" "paragraph" $ do
          el "strong" $ text $ showt c
          text " / "
          text $ showt len

      dynDone <- holdDyn False eDone
      dyn_ $ dynDone <&> \bDone ->
          when bDone $ elClass "div" "small anthrazit" $ text
              "Cleared. Press any key to start over."

      pure $ void $ filter id eDone
