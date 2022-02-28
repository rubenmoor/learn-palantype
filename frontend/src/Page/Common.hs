{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Page.Common where

import Client (postRender)
import Common.Route (FrontendRoute (..))
import Common.Stage (stageMeta)
import Control.Applicative (Applicative (pure))
import Control.Category (Category (id))
import Control.Lens
    ( (%~),
      (.~),
      (<&>),
    )
import Control.Monad (when)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Random (evalRand, newStdGen)
import Control.Monad.Reader
    ( MonadReader,
      asks,
    )
import Data.Bool (Bool (..))
import Data.Either (Either (..))
import Data.Eq (Eq ((==)))
import Data.Foldable (for_, traverse_)
import Data.Foldable (Foldable (null))
import Data.Function (($))
import Data.Functor
    ( ($>),
      void,
    )
import Data.Functor ((<$>))
import Data.Generics.Product (field)
import Data.Int (Int)
import Data.List ((!!))
import Data.List (intersperse)
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Maybe (Maybe (..))
import Data.Monoid (Monoid (mempty))
import Data.Ord ((<), Ord ((>)))
import Data.Semigroup
    ( Endo (..),
      Semigroup ((<>)),
    )
import qualified Data.Set as Set
import Data.Text (Text, toLower)
import Data.Text (length)
import Data.Witherable (Filterable (filter))
import GHC.Enum (Enum (succ), pred)
import GHC.Float (Double)
import GHC.Num (Num ((+)))
import GHC.Real ((/), fromIntegral)
import Obelisk.Route.Frontend
    ( R,
      RouteToUrl,
      SetRoute (setRoute),
      routeLink,
      pattern (:/),
    )
import Palantype.Common
    ( Chord,
      Lang (..),
      Palantype,
      PatternPos,
      fromChord,
      unparts,
    )
import Palantype.Common
    ( RawSteno,
      parseSteno,
    )
import Palantype.Common (kiBackUp, kiEnter)
import qualified Palantype.Common.Indices as KI
import Reflex.Dom
    (holdUniqDyn, (=:),
      DomBuilder,
      EventName (Click),
      EventWriter,
      HasDomEvent (domEvent),
      MonadHold (holdDyn),
      PostBuild (getPostBuild),
      Prerender,
      Reflex (Event, never),
      blank,
      dyn_,
      el,
      elAttr,
      elClass,
      elClass',
      foldDyn,
      leftmost,
      performEvent,
      text,
      updated,
    )
import Safe (atMay, initMay)
import Shared
    ( dynSimple,
      iFa,
      whenJust,
    )
import State
    ( Env (..),
      Message (..),
      Navigation (..),
      State,
      stageUrl,
      updateState,
    )
import System.Random.Shuffle (shuffleM)
import TextShow (showt)
import Text.Read (read)

elFooter ::
    forall t (m :: * -> *).
    ( DomBuilder t m,
      Prerender t m,
      RouteToUrl (R FrontendRoute) m,
      SetRoute t (R FrontendRoute) m
    ) =>
    Lang ->
    Navigation ->
    m ()
elFooter lang Navigation {..} = elClass "footer" "stage" $ do
    whenJust navMPrevious $ \prv -> do
        elClass "div" "floatLeft" $ do
            text "< "
            routeLink (stageUrl lang prv) $ text $ showt $ stageMeta prv
    text $ showt $ stageMeta navCurrent
    whenJust navMNext $ \nxt -> do
        elClass "div" "floatRight" $ do
            routeLink (stageUrl lang nxt) $ text $ showt $ stageMeta nxt
            text " >"
    elClass "br" "clearBoth" blank

elBackUp ::
    forall key (m :: * -> *) t.
    ( DomBuilder t m,
      Palantype key
    ) =>
    m ()
elBackUp =
    elClass "span" "btnSteno"
        $ text
        $ "↤ " <> showt (KI.toRaw @key kiBackUp) -- U+21A4

elCongraz ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      Palantype key,
      PostBuild t m,
      SetRoute t (R FrontendRoute) m
    ) =>
    Event t () ->
    Navigation ->
    m ()
elCongraz eDone Navigation {..} = mdo
    eChord <- asks envEChord
    let eChordEnter = void $ filter (\c -> KI.fromChord c == kiEnter) eChord
        eChordBackUp = void $ filter (\c -> KI.fromChord c == kiBackUp) eChord

    dynShowCongraz <- holdDyn False $ leftmost [eDone $> True, eBack $> False]
    eBack <- dynSimple $
        dynShowCongraz <&> \case
            False -> pure never
            True -> elClass "div" "mkOverlay" $ elClass "div" "congraz" $ do
                el "div" $ text "Task cleared!"
                el "div" $ iFa "fas fa-check-circle"
                whenJust navMNext $ \nxt -> do
                    (elACont, _) <- elClass "div" "anthrazit" $ do
                        text "Type "
                        elClass "span" "btnSteno" $ do
                            el "em" $ text "Enter "
                            el "code" $ text $ showt $ KI.toRaw @key kiEnter
                        text " to continue to "
                        elClass' "a" "normalLink" $ text $ showt $ stageMeta nxt
                    let eContinue = leftmost [eChordEnter, domEvent Click elACont]
                    updateState $
                        eContinue
                            $> [ field @"stProgress"
                                     %~ Map.update
                                         (\s -> if nxt > s then Just nxt else Just s)
                                         navLang,
                                 field @"stCleared" %~ Set.insert navCurrent,
                                 if nxt == read "stage_2-1"
                                     then field @"stTOCShowStage2" .~ True
                                     else id
                               ]
                    setRoute $ eContinue $> FrontendRoute_Main :/ ()
                el "div" $ do
                    el "span" $ text "("
                    (elABack, _) <- elClass' "a" "normalLink" $ text "back"
                    text " "
                    elBackUp @key
                    el "span" $ text ")"
                    pure $ leftmost [eChordBackUp, domEvent Click elABack]
    blank

parseStenoOrError ::
    forall proxy key t (m :: * -> *).
    (EventWriter t (Endo State) m, Palantype key, PostBuild t m) =>
    proxy key ->
    RawSteno ->
    m (Maybe [Chord key])
parseStenoOrError _ raw = case parseSteno raw of
    Right words -> pure $ Just words
    Left err -> do
        ePb <- getPostBuild
        let msgCaption = "Internal error"
            msgBody =
                "Could not parse steno code: " <> showt raw <> "\n" <> err
        updateState $ ePb $> [field @"stMsg" .~ Just Message {..}]
        pure Nothing

elNotImplemented :: forall (m :: * -> *) t. DomBuilder t m => m ()
elNotImplemented = elClass "blockquote" "warning" $ do
    el "strong" $ text "Not implemented"
    el "br" blank
    text
        "You are currently looking at an exercise that has not been \
        \implemented for the original English palantype. \
        \Feel free to read, but don't expect things to work from here on."

rawToggleKeyboard :: Lang -> RawSteno
rawToggleKeyboard = \case
    DE -> "ULNSD"
    EN -> "ALFTS"

loading ::
    forall (m :: * -> *) t.
    ( DomBuilder t m
    ) =>
    m ()
loading =
    elClass "div" "paragraph" $ do
        iFa "fas fa-spinner fa-spin"
        text " Loading ..."

data StenoWordsState = StenoWordsState
    { swsCounter :: Int,
      swsDone :: Bool,
      swsChords :: [RawSteno],
      swsWords :: [Text],
      swsNMistakes :: Int,
      swsMHint :: Maybe [RawSteno]
    }

taskWords ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      Palantype key,
      PostBuild t m,
      Prerender t m
    ) =>
    Map RawSteno Text ->
    Map Text [RawSteno] ->
    m (Event t ())
taskWords mapStenoWord mapWordStenos = do
    eChord <- asks envEChord

    eStdGen <- postRender $ do
        ePb <- getPostBuild
        performEvent $ ePb $> liftIO newStdGen

    dynMStdGen <- holdDyn Nothing $ Just <$> eStdGen

    dynSimple $
        dynMStdGen <&> \case
            Nothing -> pure never
            Just stdGen -> do
                let len = Map.size mapWordStenos
                    step :: Chord key -> StenoWordsState -> StenoWordsState
                    step c ls@StenoWordsState {..} =
                        case (fromChord c, swsWords `atMay` swsCounter) of
                            -- reset after done
                            (_, Nothing) ->
                                let words' = evalRand (shuffleM swsWords) stdGen
                                 in ls
                                        { swsDone = False,
                                          swsCounter = 0,
                                          swsWords = words',
                                          swsMHint = Nothing
                                        }
                            -- undo last input
                            (r, Just word) | r == KI.toRaw @key kiBackUp ->
                                case initMay swsChords of
                                    Just cs ->
                                        ls
                                            { swsChords = cs,
                                              swsNMistakes = succ swsNMistakes,
                                              swsMHint = Nothing
                                            }
                                    Nothing ->
                                        ls { swsMHint = Just $ Map.findWithDefault [] word mapWordStenos
                                           }
                            (raw, Just word) ->
                                let rawWord = unparts $ swsChords <> [raw]
                                    isCorrect = Map.findWithDefault "" rawWord mapStenoWord == word
                                 in if isCorrect
                                        then -- correct
                                            ls { swsDone = swsCounter == pred len
                                               , swsCounter = swsCounter + 1
                                               , swsMHint = Nothing
                                               , swsChords = []
                                               }
                                        else
                                            ls { swsChords = swsChords <> [raw] }
                    stepInitial =
                        StenoWordsState
                            { swsCounter = 0,
                              swsChords = [],
                              swsDone = False,
                              swsWords = evalRand (shuffleM $ Map.keys mapWordStenos) stdGen,
                              swsNMistakes = 0,
                              swsMHint = Nothing
                            }

                dynStenoWords <- foldDyn step stepInitial eChord

                dynDone <- holdUniqDyn $ swsDone <$> dynStenoWords
                let eDone = updated dynDone

                elClass "div" "taskWords"
                    $ dyn_
                    $ dynStenoWords <&> \StenoWordsState {..} -> do
                        elClass "span" "word"
                            $ when (swsCounter < len)
                            $ elClass "div" "exerciseField"
                            $ el "code"
                            $ text
                            $ swsWords
                                !! swsCounter

                        elClass "span" "input" $
                            for_ ( intersperse "/" $ (showt <$> swsChords) <> [" …"]
                                 ) $ \str -> el "code" $ text str

                        el "span" $ do
                            elClass "span" "btnSteno" $ text $
                                "↤ " <> showt (KI.toRaw @key kiBackUp) -- U+21A4
                            elClass "span" "small" $ text $
                                if null swsChords
                                    then " to show hint"
                                    else " to back up"

                        whenJust swsMHint $ \hint ->
                            elClass "div" "small" $ for_ hint $ \r -> do
                                text $ showt r
                                el "br" blank

                let dynCounter = swsCounter <$> dynStenoWords
                dyn_ $ dynCounter <&> \c ->
                    elClass "div" "paragraph" $ do
                        el "strong" $ text $ showt c
                        text " / "
                        text $ showt len

                dyn_ $ dynDone <&> \bDone ->
                    when bDone $ elClass "div" "small anthrazit" $
                        text "Cleared. Press any key to start over."

                pure $ void $ filter id eDone

elPatterns ::
    forall (m :: * -> *) t.
    DomBuilder t m =>
    [(PatternPos, [(Text, RawSteno)])] ->
    m ()
elPatterns doc =
    elClass "div" "patternTable" $ traverse_ elPatterns' doc
    where
        elPatterns' (pPos, pairs) = do
            let strPPos = toLower $ showt pPos
            elClass "hr" strPPos blank
            elClass "span" ("patternPosition " <> strPPos) $ text strPPos
            elClass "br" "clearBoth" blank
            for_ pairs $ \(orig, steno) ->
                elClass "div" "floatLeft" $ do
                    let lOrig :: Double = fromIntegral $ length orig
                        styleOrig =
                            if lOrig > 6
                                then "style" =: ("font-size: " <> showt ((1 + 6 / lOrig) / 2) <> "em")
                                else mempty
                        lSteno :: Double = fromIntegral $ length $ showt steno
                        styleSteno =
                            if lSteno > 6
                                then "style" =: ("font-size: " <> showt (6 / lSteno) <> "em")
                                else mempty
                    elAttr "div" ("class" =: "orig" <> styleOrig) $ text orig
                    elAttr "code" ("class" =: "steno" <> styleSteno) $ text $ showt steno
            elClass "br" "clearBoth" blank
