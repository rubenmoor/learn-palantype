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
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternGuards #-}
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
import           Control.Monad                  ( when
                                                )
import           Control.Monad.Fix              ( MonadFix )
import Control.Monad.IO.Class ( liftIO, MonadIO )
import           Control.Monad.Random           ( evalRand
                                                , newStdGen
                                                )
import           Control.Monad.Reader           ( MonadReader(ask)
                                                )
import           Data.Bool                      (not,  Bool(..), otherwise )
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
                                                , zip, sortOn
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

                                                , dynText
                                                , dyn_
                                                , el
                                                , elClass
                                                , elDynClass
                                                , foldDyn
                                                , performEvent
                                                , text
                                                , widgetHold_, TriggerEvent, PerformEvent, Performable, splitE

                                                )
import           State                          ( Env(..)
                                                , State(..)
                                                , updateState
                                                )
import           System.Random.Shuffle          ( shuffleM )
import           Text.Show                      ( Show(show) )
import           TextShow                       ( showt )
import CMS (elCMS, elCMSContent)
import Palantype.EN.Keys (palanRank)
import qualified Palantype.EN as EN
import qualified Palantype.DE as DE
import           Type.Reflection                ( (:~~:)(HRefl)
                                                , eqTypeRep
                                                , typeRep
                                                )
import Palantype.Common.TH (failure)

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
    => m ()
exercise1 = mdo

    Env {..} <- ask

    (evPart1, evPart2) <- elCMS 2 <&> splitE . mapMaybe \case
      [p1, p2] -> Just (p1, p2)
      _        -> Nothing

    elCMSContent evPart1

    evLoadedAndBuilt <- envGetLoadedAndBuilt
    updateState $ evLoadedAndBuilt $> [field @"stApp" . field @"stKeyboard" . field @"stShow" .~ True]

    evDone <- taskAlphabet (gate (not <$> current dynDone) $ catMaybes envEvMChord) True

    dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

    elCMSContent evPart2

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
    => m ()
exercise2 = mdo

    Env {..} <- ask

    (evPart1, evPart2) <- elCMS 2 <&> splitE . mapMaybe \case
      [p1, p2] -> Just (p1, p2)
      _        -> Nothing

    elCMSContent evPart1

    evDone <- taskAlphabet (gate (not <$> current dynDone) $ catMaybes envEvMChord) False
    dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

    elCMSContent evPart2

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
    => m ()
exercise3 = mdo

    Env {..} <- ask

    (evPart1, evPart2) <- elCMS 2 <&> splitE . mapMaybe \case
      [p1, p2] -> Just (p1, p2)
      _        -> Nothing

    elCMSContent evPart1

    evLoadedAndBuilt <- envGetLoadedAndBuilt
    updateState $ evLoadedAndBuilt $> [field @"stApp" . field @"stKeyboard" . field @"stShow" .~ False]

    evDone <- taskAlphabet (gate (not <$> current dynDone) $ catMaybes envEvMChord) True
    dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

    elCMSContent evPart2


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
    => m ()
exercise4 = mdo

    Env {..} <- ask

    (evPart1, evPart2) <- elCMS 2 <&> splitE . mapMaybe \case
      [p1, p2] -> Just (p1, p2)
      _        -> Nothing

    elCMSContent evPart1

    evLoadedAndBuilt <- envGetLoadedAndBuilt
    updateState $ evLoadedAndBuilt $> [field @"stApp" . field @"stKeyboard" . field @"stShow" .~ False]

    evDone <- taskAlphabet (gate (not <$> current dynDone) $ catMaybes envEvMChord) False
    dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

    elCMSContent evPart2

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

    let
        keys = if
          | Just HRefl <- typeRep @key `eqTypeRep` typeRep @EN.Key -> sortOn palanRank allKeys
          | Just HRefl <- typeRep @key `eqTypeRep` typeRep @DE.Key -> allKeys
          | otherwise -> $failure "Key not implemented"

        len = length keys

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
                ([l], _, _) | keys !! wsCounter == l -> ws
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

    elClass "div" "my-2 bg-zinc-200 rounded w-fit p-1 text-lg"
      $ elClass "code" "font-mono" $ do
        let clsLetter = if showAlphabet then "" else "text-transparent"
        for_ (zip [0 :: Int ..] keys) $ \(i, c) -> do
            let
                dynCls = dynWalk <&> \WalkState {..} -> case wsMMistake of
                    Just (j, _) -> if i == j then "bg-red-500" else clsLetter
                    Nothing ->
                        if wsCounter > i then "bg-green-500" else clsLetter
            elDynClass "span" dynCls $ elClass "span" "px-0.5"
              $ text $ Text.singleton $ keyCode c
    el "p" $ do
        dynText $ dynWalk <&> \WalkState {..} -> Text.pack $ show wsCounter
        text $ " / " <> Text.pack (show len)

    let eMistake = wsMMistake <$> updated dynWalk
        elEmptyLine = elClass "p" "text-sm mb-2" $ text " "
    widgetHold_  elEmptyLine $ eMistake <&> \case
        Just (_, w) ->
            elClass "p" "text-red-500 text-sm mb-2"
                $  text
                $  "You typed "
                <> showt w
                <> ". Any key to start over."
        Nothing -> elEmptyLine

    dynDone <- holdDyn False eDone
    dyn_ $ dynDone <&> \bDone ->
        when bDone $ elClass "div" "text-sm text-grayishblue-900" $ text
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

    eStdGen <- postRender do
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
                        Just _  -> "bg-red-500"
                when (slsCounter < len)
                    $  elClass "div" "my-2 bg-zinc-200 rounded w-fit p-1 text-lg"
                    $  elClass "code" clsMistake
                    $  text
                    $  showt
                    $  slsLetters !! slsCounter
                el "p" $ do
                    el "strong" $ text (Text.pack $ show slsCounter)
                    text " / "
                    text (Text.pack $ show len)

            let eMMistake = slsMMistake <$> updated dynStenoLetters
                elEmptyLine = elClass "p" "text-sm mb-2" $ text " "
            widgetHold_ elEmptyLine $ eMMistake <&> \case
                Just (_, chord) ->
                    elClass "div" "text-red-500 text-sm"
                        $  text
                        $  "You typed "
                        <> showt chord
                        <> ". Any key to start over."
                Nothing -> elEmptyLine

            dynDone <- holdDyn False eDone
            dyn_ $ dynDone <&> \bDone ->
                when bDone $ elClass "div" "text-sm text-grayishblue-900" $ text
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
    => m ()
exercise5 = mdo

    Env {..} <- ask

    (evPart1, evPart2) <- elCMS 2 <&> splitE . mapMaybe \case
      [p1, p2] -> Just (p1, p2)
      _        -> Nothing

    elCMSContent evPart1

    evLoadedAndBuilt <- envGetLoadedAndBuilt
    updateState $ evLoadedAndBuilt $> [field @"stApp" . field @"stKeyboard" . field @"stShow" .~ True]

    let fingersLeft = [LeftPinky, LeftRing, LeftMiddle, LeftIndex, LeftThumb]
        leftHand =
            filter (\k -> toFinger k `elem` fingersLeft) allKeys

    evDone <- taskLetters (gate (not <$> current dynDone) $ catMaybes envEvMChord) leftHand
    dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

    elCMSContent evPart2

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
    => m ()
exercise6 = mdo

    Env {..} <- ask

    (evPart1, evPart2) <- elCMS 2 <&> splitE . mapMaybe \case
      [p1, p2] -> Just (p1, p2)
      _        -> Nothing

    elCMSContent evPart1

    let fingersRight =
            [RightPinky, RightRing, RightMiddle, RightIndex, RightThumb]
        rightHand =
            filter (\k -> toFinger k `elem` fingersRight) allKeys

    evDone <- taskLetters (gate (not <$> current dynDone) $ catMaybes envEvMChord) rightHand
    dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

    elCMSContent evPart2

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
    => m ()
exercise7 = mdo

    Env {..} <- ask

    (evPart1, evPart2) <- elCMS 2 <&> splitE . mapMaybe \case
      [p1, p2] -> Just (p1, p2)
      _        -> Nothing

    elCMSContent evPart1

    let homeRow = fromIndex <$> [2, 5, 8, 11, 15, 18, 22, 25, 28, 31]

    evDone <- taskLetters (gate (not <$> current dynDone) $ catMaybes envEvMChord) homeRow
    dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

    elCMSContent evPart2

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
    => m ()
exercise8 = mdo

    Env {..} <- ask

    (evPart1, evPart2) <- elCMS 2 <&> splitE . mapMaybe \case
      [p1, p2] -> Just (p1, p2)
      _        -> Nothing

    elCMSContent evPart1

    evDone <- taskLetters (gate (not <$> current dynDone) $ catMaybes envEvMChord) allKeys
    dynDone <- elCongraz (evDone $> Nothing) (constDyn []) envNavigation

    elCMSContent evPart2
