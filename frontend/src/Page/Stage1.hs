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
import           Control.Monad                  ( when
                                                )
import           Control.Monad.Fix              ( MonadFix )
import Control.Monad.IO.Class ( liftIO, MonadIO )
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
import           Witherable                ( Filterable(catMaybes, filter, mapMaybe)
                                                )
import           GHC.Num                        ( Num((+), (-)) )
import           Obelisk.Route.Frontend         (R , SetRoute
                                                )
import           Page.Common                    ( elCongraz
                                                )
import           Palantype.Common               (keyCode,  Chord(..)
                                                , Finger(..)
                                                , Palantype(toFinger)
                                                , fromIndex
                                                , allKeys
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
                                                , widgetHold_, TriggerEvent, PerformEvent, Performable

                                                )
import           State                          ( Env(..)
                                                , Navigation(..)
                                                , State(..)
                                                , updateState
                                                )
import           System.Random.Shuffle          ( shuffleM )
import           Text.Show                      ( Show(show) )
import           TextShow                       ( showt )
import CMS (elCMS)
import Reflex.Dom.Pandoc (elPandoc, defaultConfig)

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
       , MonadIO (Performable m)
       , MonadReader (Env t key) m
       , Palantype key
       , PerformEvent t m
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       , TriggerEvent t m
       )
    => m Navigation
exercise1 = do

    Env {..} <- ask

    evParts <- elCMS 2 <&> mapMaybe \case
      [p1, p2] -> Just (p1, p2)
      _        -> Nothing
    widgetHold_ blank $ evParts <&> \(part1, part2) -> mdo
      elPandoc defaultConfig part1

      evLoadedAndBuilt <- envGetLoadedAndBuilt
      updateState $ evLoadedAndBuilt $> [field @"stApp" . field @"stShowKeyboard" .~ True]

      evDone <- taskAlphabet (gate (not <$> current dynDone) envEChord) True

      dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

      elPandoc defaultConfig part2

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
       , SetRoute t (R FrontendRoute) m
       , MonadIO (Performable m)
       , PerformEvent t m
       , TriggerEvent t m
       )
    => m Navigation
exercise2 = mdo

    Env {..} <- ask

    evParts <- elCMS 2 <&> mapMaybe \case
      [p1, p2] -> Just (p1, p2)
      _        -> Nothing
    widgetHold_ blank $ evParts <&> \(part1, part2) -> mdo
      elPandoc defaultConfig part1

      evLoadedAndBuilt <- envGetLoadedAndBuilt
      updateState $ evLoadedAndBuilt $> [field @"stApp" . field @"stShowKeyboard" .~ True]

      evDone <- taskAlphabet (gate (not <$> current dynDone) envEChord) False
      dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

      elPandoc defaultConfig part2

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
       , SetRoute t (R FrontendRoute) m
       , MonadIO (Performable m)
       , PerformEvent t m
       , TriggerEvent t m
       )
    => m Navigation
exercise3 = mdo

    Env {..} <- ask

    evParts <- elCMS 2 <&> mapMaybe \case
      [p1, p2] -> Just (p1, p2)
      _        -> Nothing
    widgetHold_ blank $ evParts <&> \(part1, part2) -> mdo
      elPandoc defaultConfig part1

      evLoadedAndBuilt <- envGetLoadedAndBuilt
      updateState $ evLoadedAndBuilt $> [field @"stApp" . field @"stShowKeyboard" .~ False]

      evDone <- taskAlphabet (gate (not <$> current dynDone) envEChord) True
      dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

      elPandoc defaultConfig part2
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
       , SetRoute t (R FrontendRoute) m
       , TriggerEvent t m
       , PerformEvent t m
       , MonadIO (Performable m)
       )
    => m Navigation
exercise4 = mdo

    Env {..} <- ask

    evParts <- elCMS 2 <&> mapMaybe \case
      [p1, p2] -> Just (p1, p2)
      _        -> Nothing
    widgetHold_ blank $ evParts <&> \(part1, part2) -> mdo
      elPandoc defaultConfig part1

      evLoadedAndBuilt <- envGetLoadedAndBuilt
      updateState $ evLoadedAndBuilt $> [field @"stApp" . field @"stShowKeyboard" .~ False]

      evDone <- taskAlphabet (gate (not <$> current dynDone) envEChord) False
      dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

      elPandoc defaultConfig part2

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
       , SetRoute t (R FrontendRoute) m
       , TriggerEvent t m
       , PerformEvent t m
       , MonadIO (Performable m)
       )
    => m Navigation
exercise5 = mdo

    Env {..} <- ask

    evParts <- elCMS 2 <&> mapMaybe \case
      [p1, p2] -> Just (p1, p2)
      _        -> Nothing
    widgetHold_ blank $ evParts <&> \(part1, part2) -> mdo
      elPandoc defaultConfig part1

      evLoadedAndBuilt <- envGetLoadedAndBuilt
      updateState $ evLoadedAndBuilt $> [field @"stApp" . field @"stShowKeyboard" .~ True]

      let fingersLeft = [LeftPinky, LeftRing, LeftMiddle, LeftIndex, LeftThumb]
          leftHand =
              filter (\k -> toFinger k `elem` fingersLeft) allKeys

      evDone <- taskLetters (gate (not <$> current dynDone) envEChord) leftHand
      dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

      elPandoc defaultConfig part2

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
       , SetRoute t (R FrontendRoute) m
       , TriggerEvent t m
       , PerformEvent t m
       , MonadIO (Performable m)
       )
    => m Navigation
exercise6 = mdo

    Env {..} <- ask

    evParts <- elCMS 2 <&> mapMaybe \case
      [p1, p2] -> Just (p1, p2)
      _        -> Nothing
    widgetHold_ blank $ evParts <&> \(part1, part2) -> mdo
      elPandoc defaultConfig part1

      evLoadedAndBuilt <- envGetLoadedAndBuilt
      updateState $ evLoadedAndBuilt $> [field @"stApp" . field @"stShowKeyboard" .~ True]

      let fingersRight =
              [RightPinky, RightRing, RightMiddle, RightIndex, RightThumb]
          rightHand =
              filter (\k -> toFinger k `elem` fingersRight) allKeys

      evDone <- taskLetters (gate (not <$> current dynDone) envEChord) rightHand
      dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

      elPandoc defaultConfig part2

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
       , SetRoute t (R FrontendRoute) m
       , TriggerEvent t m
       , PerformEvent t m
       , MonadIO (Performable m)
       )
    => m Navigation
exercise7 = mdo

    Env {..} <- ask

    evParts <- elCMS 2 <&> mapMaybe \case
      [p1, p2] -> Just (p1, p2)
      _        -> Nothing
    widgetHold_ blank $ evParts <&> \(part1, part2) -> mdo
      elPandoc defaultConfig part1

      evLoadedAndBuilt <- envGetLoadedAndBuilt
      updateState $ evLoadedAndBuilt $> [field @"stApp" . field @"stShowKeyboard" .~ True]

      let homeRow = fromIndex <$> [2, 5, 8, 11, 15, 18, 22, 25, 28, 31]

      evDone <- taskLetters (gate (not <$> current dynDone) envEChord) homeRow
      dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

      elPandoc defaultConfig part2

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
       , SetRoute t (R FrontendRoute) m
       , MonadIO (Performable m)
       , PerformEvent t m
       , TriggerEvent t m
       )
    => m Navigation
exercise8 = mdo

    Env {..} <- ask

    evParts <- elCMS 2 <&> mapMaybe \case
      [p1, p2] -> Just (p1, p2)
      _        -> Nothing
    widgetHold_ blank $ evParts <&> \(part1, part2) -> mdo
      elPandoc defaultConfig part1

      evLoadedAndBuilt <- envGetLoadedAndBuilt
      updateState $ evLoadedAndBuilt $> [field @"stApp" . field @"stShowKeyboard" .~ True]

      evDone <- taskLetters (gate (not <$> current dynDone) envEChord) allKeys
      dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

      elPandoc defaultConfig part2
    pure envNavigation
