{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TemplateHaskell #-}

module Page.Stage1 where

import           Client                         ( postRender )
import           Common.Route                   ( FrontendRoute(..) )
import           Control.Applicative            ( (<$>)
                                                , Applicative(pure)
                                                )
import           Control.Category               (Category((.), id) )
import           Control.Lens                   ((.~)
                                                , (<&>)
                                                )
import           Control.Monad                  ( unless
                                                , when
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Random           ( evalRand
                                                , newStdGen
                                                )
import           Control.Monad.Reader           ( MonadReader(ask)
                                                )
import           Data.Bool                      (not,  Bool(..) )
import           Data.Eq                        ( Eq((==)) )
import           Data.Foldable                  ( Foldable(elem, length)
                                                , for_
                                                )
import           Data.Function                  (($) )
import           Data.Functor                   ( ($>)
                                                , Functor(fmap)
                                                , void
                                                )
import           Data.Generics.Product          ( field )
import           Data.Int                       ( Int )
import           Data.List                      ( (!!)
                                                , zip
                                                )
import           Data.Maybe                     ( Maybe(..) )
import           Data.Ord                       ( Ord((<), (>)) )
import           Data.Semigroup                 ( Endo(..)
                                                , Semigroup((<>))
                                                )
import qualified Data.Text                     as Text
import           Data.Witherable                ( Filterable(catMaybes, filter)
                                                )
import           GHC.Num                        ( Num((+), (-)) )
import           Obelisk.Route.Frontend         (Routed,  R
                                                , SetRoute
                                                )
import           Page.Common                    ( elCongraz
                                                , elNotImplemented
                                                )
import           Palantype.Common               (keyCode, Lang(..),  Chord(..)
                                                , Finger(..)
                                                , Palantype(toFinger)
                                                , fromIndex
                                                , allKeys
                                                , kiInsert
                                                )
import           Reflex.Dom                     (constDyn, current, gate, never, switchDyn, widgetHold,  DomBuilder
                                                , EventWriter
                                                , MonadHold(holdDyn)
                                                , PostBuild(getPostBuild)
                                                , Prerender
                                                , Reflex
                                                    ( Event
                                                    , updated
                                                    )
                                                , blank
                                                , dynText
                                                , dyn_
                                                , el
                                                , elClass
                                                , elDynClass
                                                , foldDyn
                                                , performEvent
                                                , text
                                                , widgetHold_

                                                )
import           State                          ( Env(..)
                                                , Navigation(..)
                                                , State(..)
                                                , updateState
                                                )
import           System.Random.Shuffle          ( shuffleM )
import           Text.Show                      ( Show(show) )
import           TextShow                       ( showt )
import Common.Stage (StageIndex, Stage)
import qualified Palantype.Common.Indices as KI

-- exercise 1

data WalkState k = WalkState
    { wsCounter  :: Int
    , wsMMistake :: Maybe (Int, Chord k)
    , wsDone     :: Maybe Bool
    }

exercise1
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , Routed t StageIndex m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise1 = mdo

    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang `elem` [DE, EN]) elNotImplemented

    el "h1" $ text "Stage 1"
    el "h3" $ text "Exercise 1"
    el "h2" $ text "The Palantype Alphabet"
    el "p" $ text
      "Palantype relies on chords. A chord means: You press up to ten keys at \
      \the same time. The order in which you press down does not matter. \
      \Instead, all the letters of one chord will appear in their proper order. \
      \Therefore, you will learn the Palantype Alphabet in its proper order now."
    el "p" $ text
        "Type the following steno letters in order, one after another."
    el "p" $ text
      "Some letters occur twice, the first time for your left hand \
      \and the second time for your right hand."

    ePb <- getPostBuild
    updateState $ ePb $> [field @"stApp" . field @"stShowKeyboard" .~ True]

    evDone <- taskAlphabet (gate (not <$> current dynDone) envEChord) True

    dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

    when (navLang == DE) $ do
      el "p" $ text
            "In case you wonder: The letter ʃ is a phonetic symbol \
            \related to the German \"sch\". \
            \We don't care about exact phonetics and thus simply treat ʃ \
            \as \"sch\"."
      el "p" $ do
        text "Also, the keys with small letters are special. "
        el "code" $ text "v"
        text ", "
        el "code" $ text "b"
        text ", and "
        el "code" $ text "n"
        text " stand for \"ver-\", \"be-\", and \"-en\", respectively. \
             \These keys aren't necessary and you won't use them in a while. \
             \E.g. \"be\" can be typed simply by typing "
        el "code" $ text "BE"
        text ". But given the high frequency of these word parts, these \
            \ special keys will help a lot with typing efficiency. The "
        el "code" $ text "s"
        text " key (small s) is a specical key among the special keys and has \
            \ whole exercises dedicated to its use."

    pure envNavigation

-- 1.2

exercise2
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , Routed t StageIndex m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise2 = mdo

    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang `elem` [DE, EN]) elNotImplemented

    el "h1" $ text "Stage 1"
    el "h3" $ text "Exercise 2"
    el "h2" $ text "The Palantype Alphabet"
    el "p" $ text
      "Again, type the letters in the Palantype Alphabet. \
      \But now, without seeing them. \
      \Learn to remember the correct order by \
      \pronouncing each letter while you type it!"

    ePb <- getPostBuild
    updateState $ ePb $> [field @"stApp" . field @"stShowKeyboard" .~ True]

    evDone <- taskAlphabet (gate (not <$> current dynDone) envEChord) False
    dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

    case navLang of
        DE -> el "p" $ do
            text "Well, how to pronounce ~? This symbol is used to turn "
            el "em" $ text "u"
            text " into "
            el "em" $ text "uh"
            text ", "
            el "em" $ text "i"
            text " into "
            el "em" $ text "ie"
            text ", "
            el "em" $ text "o"
            text " into "
            el "em" $ text "oh"
            text ", and "
            el "em" $ text "ü"
            text " into "
            el "em" $ text "üh"
            text ". It is called «lang» (the German word for long)."
        EN ->
            el "p"
                $ text
                      "The + is called cross and the ^ is called point, \
              \in case you wondered."

    pure envNavigation

-- 1.3

exercise3
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , Routed t StageIndex m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise3 = mdo

    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang `elem` [DE, EN]) elNotImplemented

    el "h1" $ text "Stage 1"
    el "h3" $ text "Exercise 3"
    el "h2" $ text "The Palantype Alphabet"
    el "p"
        $ text
              "How about you type the letters in their proper order \
      \without the virtual keyboard? \
      \Again, get used to remembering them!"

    ePb <- getPostBuild
    updateState $ ePb $> [field @"stApp" . field @"stShowKeyboard" .~ False]

    evDone <- taskAlphabet (gate (not <$> current dynDone) envEChord) True
    dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

    case navLang of
        DE -> do
          el "p" $ do
            text "Missing the letters "
            el "em" $ text "C"
            text ", "
            el "em" $ text "H"
            text ", "
            el "em" $ text "J"
            text ", "
            el "em" $ text "K"
            text ", "
            el "em" $ text "P"
            text ", "
            el "em" $ text "Q"
            text ", "
            el "em" $ text "R"
            text ", "
            el "em" $ text "T"
            text ", "
            el "em" $ text "V"
            text ", "
            el "em" $ text "W"
            text ", "
            el "em" $ text "X"
            text ", "
            el "em" $ text "Y"
            text ", and "
            el "em" $ text "Z"
            text "? The choice of letters in the Palantype alphabet is based \
                 \on their frequency in written German. Only the most common \
                 \letters are represented directly. It's worth mentioning that \
                 \the missing letters "
            el "em" $ text "K"
            text ", "
            el "em" $ text "P"
            text ", and "
            el "em" $ text "T"
            text " are quite common, too, and you will learn that they can be \
                 \reached fairly easily by combining either one of "
            el "em" $ text "G"
            text ", "
            el "em" $ text "B"
            text ", and "
            el "em" $ text "D"
            text " with the "
            el "code" $ text "+"
            text " key. "

          el "p" $ do
            text "Technically, "
            el "em" $ text "Ö"
            text " and "
            el "em" $ text "ß"
            text " are also missing. The less frequent a letter, the more \
                 \keys you will have to combine into one chord to reach it. \
                 \This makes any steno system extremely flexible regarding \
                 \special letters, regardless of their frequency. Palantype \
                 \DE accounts even for the french letters "
            el "em" $ text "ç"
            text ", and "
            el "em" $ text "é"
            text "."
        EN -> pure () -- TODO
    pure envNavigation

-- 1.4

exercise4
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , Routed t StageIndex m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise4 = mdo

    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang `elem` [DE, EN]) elNotImplemented

    el "h1" $ text "Stage 1"
    el "h3" $ text "Exercise 4"
    el "h2" $ text "The Palantype Alphabet"
    el "p"
        $ text
              "And for maximum difficulty, type the letters in their proper \
      \order without seeing neither the letters nor the keyboard! To makes this \
      \a bit of a challenge: First, don't look at your fingers. Second, \
      \try to speak out loud every letter, before you are going to type it."

    ePb <- getPostBuild
    updateState $ ePb $> [field @"stApp" . field @"stShowKeyboard" .~ False]

    evDone <- taskAlphabet (gate (not <$> current dynDone) envEChord) False
    dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

    el "p" $ do
      text "Remember that "
      el "code" $ text "ʃ"
      text " is used to type "
      el "em" $ text "sch"
      text "? As I told you in the last stage, less frequent letters require \
           \more keys to be pressed at once. The "
      el "code" $ text "ʃ"
      text " key is an example that the opposite is also true: By including "
      el "em" $ text "sch"
      text " as a single key in the layout, you immediately save a couple of \
           \strokes. There is, however, an even more important reason why the "
      el "code" $ text "ʃ"
      text " is included as a single key: In German, "
      el "em" $ text "sch"
      text " is combined with other letters as if it were a single letter. \
           \Think of "
      el "em" $ text "Quatsch"
      text ", "
      el "em" $ text "Schwach"
      text ", "
      el "em" $ text "Dschungel"
      text ", and "
      el "em" $ text "Plantschst"
      text "!"

    el "p" $ do
      text "Given these heavy syllable, creating steno codes that fit into \
           \single chords turns out to be a major headache. The task is \
           \simplified quite a bit by reducing the "
      el "em" $ text "sch"
      text " to a single key. This simplification is important for both, \
           \the algorithm that creates the steno codes and you, the learner \
           \that will sooner or later memorize them."

    pure envNavigation

{-|
Pass through all the letters of the steno alphabet one by one
-}
taskAlphabet
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , MonadFix m
       , MonadHold t m
       , Palantype key
       , PostBuild t m
       )
    => Event t (Chord key)
    -> Bool -- ^ show the alphabet
    -> m (Event t ())
taskAlphabet evChord showAlphabet = do

    let len = length $ allKeys @key

        step :: Chord key -> WalkState key -> WalkState key
        step (Chord ks) ws@WalkState {..} =
            case (ks, wsMMistake, wsDone) of

        -- reset after done
                (_, _, Just True) ->
                    ws { wsDone = Just False, wsCounter = 0 }

                -- reset after mistake
                (_, Just _, _) ->
                    ws { wsCounter = 0, wsMMistake = Nothing }

                -- correct
                ([l], _, _) | allKeys !! wsCounter == l -> ws
                    { wsDone    = if wsCounter == len - 1
                                      then Just True
                                      else Nothing
                    , wsCounter = wsCounter + 1
                    }

                -- mistake
                (ls, _, _) -> ws { wsDone     = Nothing
                                 , wsMMistake = Just (wsCounter, Chord ls)
                                 }

        stepInitial = WalkState { wsCounter  = 0
                                , wsMMistake = Nothing
                                , wsDone     = Nothing
                                }

    dynWalk <- foldDyn step stepInitial evChord
    let eDone = catMaybes $ wsDone <$> updated dynWalk

    elClass "div" "exerciseField" $ el "code" $ do
        let clsLetter = if showAlphabet then "" else "fgTransparent"
        for_ (zip [0 :: Int ..] $ allKeys @key) $ \(i, c) -> do
            let
                dynCls = dynWalk <&> \WalkState {..} -> case wsMMistake of
                    Just (j, _) -> if i == j then "bgRed" else clsLetter
                    Nothing ->
                        if wsCounter > i then "bgGreen" else clsLetter
            elDynClass "span" dynCls $ text $ Text.singleton $ keyCode c
    el "p" $ do
        dynText $ dynWalk <&> \WalkState {..} -> Text.pack $ show wsCounter
        text $ " / " <> Text.pack (show len)

    let eMistake = wsMMistake <$> updated dynWalk
    widgetHold_ blank $ eMistake <&> \case
        Just (_, w) ->
            elClass "div" "red small paragraph"
                $  text
                $  "You typed "
                <> showt w
                <> ". Any key to start over."
        Nothing -> blank

    dynDone <- holdDyn False eDone
    dyn_ $ dynDone <&> \bDone ->
        when bDone $ elClass "div" "small anthrazit" $ text
            "Cleared. Press any key to start over."

    pure $ void $ filter id eDone

-- stage 1.5

data StenoLettersState k = StenoLettersState
    { slsCounter  :: Int
    , slsMMistake :: Maybe (Int, Chord k)
    , slsDone     :: Maybe Bool
    , slsLetters  :: [k]
    }

{-|
Type random steno letters as they appear
-}
taskLetters
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , MonadFix m
       , MonadHold t m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       )
    => Event t (Chord key)
    -> [key]
    -> m (Event t ())
taskLetters evChord letters = do

    eStdGen <- postRender $ do
        ePb <- getPostBuild
        performEvent $ ePb $> liftIO newStdGen

    fmap switchDyn $ widgetHold (pure never) $ eStdGen <&> \stdGen -> mdo
            let len = length letters

                step
                    :: Chord key
                    -> StenoLettersState key
                    -> StenoLettersState key
                step (Chord ks) ls@StenoLettersState {..} =
                    case (ks, slsMMistake, slsDone) of

                    -- reset after done
                        (_, _, Just True) ->
                            let letters' =
                                    evalRand (shuffleM slsLetters) stdGen
                            in  ls { slsDone    = Just False
                                   , slsCounter = 0
                                   , slsLetters = letters'
                                   }

                        -- reset after mistake
                        (_, Just _, _) ->
                            ls { slsCounter = 0, slsMMistake = Nothing }

                        -- correct
                        ([l], _, _) | slsLetters !! slsCounter == l -> ls
                            { slsDone    = if slsCounter == len - 1
                                               then Just True
                                               else Nothing
                            , slsCounter = slsCounter + 1
                            }

                        -- mistake
                        (wrong, _, _) -> ls
                            { slsDone     = Nothing
                            , slsMMistake = Just (slsCounter, Chord wrong)
                            }

                stepInitial = StenoLettersState
                    { slsCounter  = 0
                    , slsMMistake = Nothing
                    , slsDone     = Nothing
                    , slsLetters  = evalRand (shuffleM letters) stdGen
                    }

            dynStenoLetters <- foldDyn step stepInitial evChord

            let eDone = catMaybes $ slsDone <$> updated dynStenoLetters

            dyn_ $ dynStenoLetters <&> \StenoLettersState {..} -> do
                let clsMistake = case slsMMistake of
                        Nothing -> ""
                        Just _  -> "bgRed"
                when (slsCounter < len)
                    $  elClass "div" "exerciseField"
                    $  elClass "code" clsMistake
                    $  text
                    $  showt
                    $  slsLetters !! slsCounter
                el "p" $ do
                    el "strong" $ text (Text.pack $ show slsCounter)
                    text " / "
                    text (Text.pack $ show len)

            let eMMistake = slsMMistake <$> updated dynStenoLetters
            widgetHold_ blank $ eMMistake <&> \case
                Just (_, chord) ->
                    elClass "div" "red small"
                        $  text
                        $  "You typed "
                        <> showt chord
                        <> ". Any key to start over."
                Nothing -> blank

            dynDone <- holdDyn False eDone
            dyn_ $ dynDone <&> \bDone ->
                when bDone $ elClass "div" "small anthrazit" $ text
                    "Cleared. Press any key to start over."

            pure $ void $ filter id eDone

exercise5
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , Prerender t m
       , PostBuild t m
       , Routed t StageIndex m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise5 = mdo

    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang `elem` [DE, EN]) elNotImplemented

    el "h1" $ text "Stage 1"
    el "h3" $ text "Exercise 5"
    el "h2" $ text "The Palantype Alphabet"
    el "p" $ do
        text
            "You get the virtual keyboard back. Feel free, to toggle it anytime. \
         \You can even use "
        el "code" $ text $ showt $ KI.toRaw @key kiInsert
        text " to do that. This is one of the "
        el "em" $ text "command keys"
        text ". You will later learn that it is meant to replace the "
        el "em" $ text "insert"
        text " key on your keyboard."
    el "p" $ text "Type every steno letter as it appears!"
    el "p" $ do
        text
            "The - symbol is used to distinguish between letters that appear \
         \twice. In this task, you will only need your left hand. Thus \
         \the letters have a trailing -."

    ePb <- getPostBuild
    updateState $ ePb $> [field @"stApp" . field @"stShowKeyboard" .~ True]

    let fingersLeft = [LeftPinky, LeftRing, LeftMiddle, LeftIndex, LeftThumb]
        leftHand =
            filter (\k -> toFinger k `elem` fingersLeft) allKeys

    evDone <- taskLetters (gate (not <$> current dynDone) envEChord) leftHand
    dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

    el "p" do
      text "Fun fact: The letter "
      el "em" $ text "t"
      text " is more common in German than "
      el "em" $ text "d"
      text ", still there is only the "
      el "code" $ text "D"
      text " key. The reason is that "
      el "em" $ text "d"
      text " is reasonably frequent in the onset (the beginning) of syllables \
           \and by encoding "
      el "em" $ text "t"
      text " as "
      el "code" $ text "D+-"
      text " and "
      el "code" $ text "-+D"
      text ", respectively, we end up with a somewhat consistent rule for the use of "
      el "code" $ text "+"
      text ": It converts the soft letters "
      el "em" $ text "b"
      text ", "
      el "em" $ text "d"
      text ", and "
      el "em" $ text "g"
      text " into their hard counterparts "
      el "em" $ text "p"
      text ", "
      el "em" $ text "t"
      text ", and "
      el "em" $ text "k"
      text "."

    el "p" do
      text "Later on you will realize that in a lot of cases, a "
      el "em" $ text "t"
      text " in the coda (the final part of a syllable) can be typed using "
      el "code" $ text "D"
      text " without the "
      el "code" $ text "+"
      text ". The concept behind all this is to have a complete rule set \
           \that allows to reach each and any German word first. Then additional \
           \rules are added to increase typing efficiency."

    pure envNavigation

exercise6
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , Prerender t m
       , PostBuild t m
       , Routed t StageIndex m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise6 = mdo

    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang `elem` [DE, EN]) elNotImplemented

    el "h1" $ text "Stage 1"
    el "h3" $ text "Exercise 6"
    el "h2" $ text "The Palantype Alphabet"
    el "p" $ do
        text
            "Switching hands now. The leading - symbol indicates that the letter \
         \is on your right-hand side."
    el "p" $ do
        text "Type every steno letter as it appears!"

    ePb <- getPostBuild
    updateState $ ePb $> [field @"stApp" . field @"stShowKeyboard" .~ True]

    let fingersRight =
            [RightPinky, RightRing, RightMiddle, RightIndex, RightThumb]
        rightHand =
            filter (\k -> toFinger k `elem` fingersRight) allKeys

    evDone <- taskLetters (gate (not <$> current dynDone) envEChord) rightHand
    dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

    el "p" do
      text "By the way, the order of keys in the Palantype alphabet underwent \
           \important optimizations, too. In the Palantype system, any finger \
           \is meant to press down only one key at a time. \
           \It follows immediately that keys that are in the same column (\
           \treating thumb rows as columns, too) cannot appear both in a single \
           \chord."

    el "p" do
      text "This is bad news for the Gnu: It can't be typed without some special \
           \rule, because "
      el "code" $ text "GNU"
      text " is not a valid chord, even though all the letters are there. The \
           \actual order of the keys tries to account for the most common letter \
           \combinations in the real German language first. A case can be made \
           \to swap the keys "
      el "code" $ text "D"
      text " and "
      el "code" $ text "S"
      text ", given the high frequency of "
      el "em" $ text "st"
      text " in the beginning and "
      el "em" $ text "ts"
      text " in the end of German words. However, the "
      el "em" $ text "t"
      text " also frequently combines with literally any other consonant."
      el "em" $ text "-st"
      text ","
      el "em" $ text "-ft"
      text ","
      el "em" $ text "-scht"
      text ","
      el "em" $ text "-lt"
      text ","
      el "em" $ text "-nt"
      text ","
      el "em" $ text "-mt"
      text ","
      el "em" $ text "-kt"
      text ", … you name it. Putting the "
      el "code" $ text "D"
      text " in the rightmost column of your right \
           \hand allows for that without any special rules. To keep the partial \
           \symmetry intact, the "
      el "code" $ text "D-"
      text " of your left hand, is also placed in the outermost column."

    pure envNavigation

exercise7
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , Prerender t m
       , PostBuild t m
       , Routed t StageIndex m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise7 = mdo

    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang `elem` [DE, EN]) elNotImplemented

    el "h1" $ text "Stage 1"
    el "h3" $ text "Exercise 7"
    el "h2" $ text "The Palantype Alphabet"
    el "p" $ do
        text
            "This one will be fun. You won't have to move your hands at all. \
            \All the keys you practice in this exercise lie on home row. \
            \If you find your hands moving, still, maybe adjust your keyboard. \
            \Home row should be reachable with as little movement as possible."
    el "p" $
      text "Be sure to practice this one to perfection. It will only get more \
           \difficult from here."

    ePb <- getPostBuild
    updateState $ ePb $> [field @"stApp" . field @"stShowKeyboard" .~ True]

    let homeRow = fromIndex <$> [2, 5, 8, 11, 15, 18, 22, 25, 28, 31]

    evDone <- taskLetters (gate (not <$> current dynDone) envEChord) homeRow

    dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

    el "p" do
      text "You probably understand now, why the special keys "
      el "code" $ text "v"
      text ", "
      el "code" $ text "b"
      text ", "
      el "code" $ text "n"
      text ", and "
      el "code" $ text "s"
      text " are placed in the outermost columns along with "
      el "code" $ text "D"
      text ". I mentioned earlier that "
      el "code" $ text "v"
      text " and "
      el "code" $ text "b"
      text " encode the common prefixes "
      el "em" $ text "ver-"
      text " and "
      el "em" $ text "be-"
      text ". "
      el "code" $ text "n"
      text " encodes the extremely frequent suffix "
      el "em" $ text "-en"
      text ". Thus, in order to reduce longish words like "
      el "em" $ text "vermischen"
      text " to one single chord, "
      el "code" $ text "v"
      text ", in principle, needs to be combined with any other letter. \
           \Conversely, any other letter at the end, in principle, needs \
           \to be combined with "
      el "code" $ text "n"
      text ", resulting in the very compact steno "
      el "code" $ text "vMIʃn"
      text " for a three syllablle word."

    pure envNavigation

exercise8
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , Prerender t m
       , PostBuild t m
       , Routed t StageIndex m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise8 = mdo

    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang `elem` [DE, EN]) elNotImplemented

    el "h1" $ text "Stage 1"
    el "h3" $ text "Exercise 8"
    el "h2" $ text "The Palantype Alphabet"
    el "p" $ do
        text
            "Before your continue with this last exercise of Stage 1: There is a \
         \table of contents on the left. Use it to jump back to any of the \
         \previous exercises to practice some more."
    el "p" $ do
        text "Type every steno letter as it appears!"
    el "p" $ text
              "By the way, you can re-shuffle the order, in which the keys \
           \are presented to you, by reloading the page, if you feel the need to."


    ePb <- getPostBuild
    updateState $ ePb $> [field @"stApp" . field @"stShowKeyboard" .~ True]

    evDone <- taskLetters (gate (not <$> current dynDone) envEChord) allKeys

    dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

    el "h3" $ text "A remark on learning strategy"
    el "p" $
      text "I personally feel more comfortable \
           \practicing whole steno chords in the upcoming exercises once I \
           \perfectly remember the position of each individual key. Every single \
           \exercise will introduce new rules to remember and I prefer not to \
           \get distracted by searching for keys and looking up at the virtual \
           \keyboard."

    el "p" $ text "Having said that, the opposite case can be argued, too: You \
           \will go through a lot of exercises, each of them repeating all of the \
           \rules again and again. By the time you reach the last exercise of \
           \Stage 3 you will have memorized the key positions regardlessly."

    el "p" $ text "The real lesson is that, quite often, different strategies \
           \work well for different persons. If you are going to master \
           \stenographic typing, you probably have a good idea about \
           \learning strategies that work for you. If you don't, I encourage you \
           \to take a break once in a while and reflect on your progress."

    el "p" $ text "The single best advice that I ever got when it comes to \
           \learning: Don't get lost in a system. Any system is just a crutch \
           \that you throw away once you learned to walk. Systems, apps and \
           \interactive tutorials like this one, can be quite devious in that \
           \they come with a reward system. The one reward that counts, however, \
           \is mastering your very own goal that led you to stenographic \
           \typing in the first place!"

    el "p" $ text "And as long as you do not lose focus regarding your original \
           \goal, everything is allowed. Following the path of your personal \
           \interests is a great way of learning, especially for a big project \
           \like learning stenographic typing."

    pure envNavigation
