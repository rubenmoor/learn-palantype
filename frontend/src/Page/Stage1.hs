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
import Common.Stage (Stage)
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
       , Routed t Stage m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise1 = mdo

    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang `elem` [DE, EN]) elNotImplemented

    el "h1" $ text "Stage 1"
    el "h2" $ text "The Palantype Alphabet"
    el "h3" $ text "Exercise 1"
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
            "In case you wonder: The letter ?? is a phonetic symbol \
            \related to the German \"sch\". \
            \We don't care about exact phonetics and thus simply treat ?? \
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
       , Routed t Stage m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise2 = mdo

    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang `elem` [DE, EN]) elNotImplemented

    el "h1" $ text "Stage 1"
    el "h2" $ text "The Palantype Alphabet"
    el "h3" $ text "Exercise 2"
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
            el "em" $ text "??"
            text " into "
            el "em" $ text "??h"
            text ". It is called ??lang?? (the German word for long)."
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
       , Routed t Stage m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise3 = mdo

    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang `elem` [DE, EN]) elNotImplemented

    el "h1" $ text "Stage 1"
    el "h2" $ text "The Palantype Alphabet"
    el "h3" $ text "Exercise 3"
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
        DE -> el "p" $ do
            text
                "Missing the letter T? It's not there and you don't need it. \
                   \You will learn to type "
            el "em" $ text "t"
            text " as "
            el "code" $ text "D+"
            text " and "
            el "em" $ text "st"
            text " as "
            el "code" $ text "DS"
            text ", when it occurs in the onset of a syllable."
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
       , Routed t Stage m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise4 = mdo

    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang `elem` [DE, EN]) elNotImplemented

    el "h1" $ text "Stage 1"
    el "h2" $ text "The Palantype Alphabet"
    el "h3" $ text "Exercise 4"
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
       , Routed t Stage m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise5 = mdo

    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang `elem` [DE, EN]) elNotImplemented

    el "h1" $ text "Stage 1"
    el "h2" $ text "The Palantype Alphabet"
    el "h3" $ text "Exercise 5"
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
       , Routed t Stage m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise6 = mdo

    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang `elem` [DE, EN]) elNotImplemented

    el "h1" $ text "Stage 1"
    el "h2" $ text "The Palantype Alphabet"
    el "h3" $ text "Exercise 6"
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
       , Routed t Stage m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise7 = mdo

    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang `elem` [DE, EN]) elNotImplemented

    el "h1" $ text "Stage 1"
    el "h2" $ text "The Palantype Alphabet"
    el "h3" $ text "Exercise 7"
    el "p" $ do
        text
            "This one will be fun. You won't have to move your hands at all. \
            \All the keys you practice in this exercise lie on home row. \
            \If you find your hands moving, still, maybe adjust your keyboard. \
            \Home row should be reachable with as little movement as possible."

    ePb <- getPostBuild
    updateState $ ePb $> [field @"stApp" . field @"stShowKeyboard" .~ True]

    let homeRow = fromIndex <$> [2, 5, 8, 11, 15, 18, 22, 25, 28, 31]

    evDone <- taskLetters (gate (not <$> current dynDone) envEChord) homeRow

    el "p"
        $ text
              "Be sure to practice this one to perfection. It will only get more \
              \difficult from here."

    dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

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
       , Routed t Stage m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise8 = mdo

    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang `elem` [DE, EN]) elNotImplemented

    el "h1" $ text "Stage 1"
    el "h2" $ text "The Palantype Alphabet"
    el "h3" $ text "Exercise 7"
    el "p" $ do
        text
            "Before your continue with this last exercise of Stage 1: There is a \
         \table of contents on the left. Use it to jump back to any of the \
         \previous exercises to practice some more."
    el "p" $ do
        text
            "For the next stage, you should have some muscle memory for every \
         \key. Be sure to complete this exercise without \
         \the virtual keyboard, too."
    el "p" $ do
        text "Type every steno letter as it appears!"

    ePb <- getPostBuild
    updateState $ ePb $> [field @"stApp" . field @"stShowKeyboard" .~ True]

    evDone <- taskLetters (gate (not <$> current dynDone) envEChord) allKeys

    el "p" $ text
              "By the way, you can re-shuffle the order, in which the keys \
           \are presented to you, by reloading the page, if you feel the need to."

    dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation
    pure envNavigation
