{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Page.Common where

import           Client                         ( postRender )
import           Common.Route                   ( FrontendRoute(..) )
import           Common.Stage                   ( stageMeta )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ((>>>), (<<<),  Category(id, (.))
                                                )
import           Control.Lens                   ( (?~)
                                                , (%~)
                                                , (.~)
                                                , (<&>)
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Random           ( evalRand
                                                , newStdGen
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                )
import           Data.Bool                      ((||),  Bool(..)
                                                )
import           Data.Either                    ( Either(..) )
import           Data.Eq                        ( Eq((==)) )
import           Data.Foldable                  ( for_
                                                , traverse_
                                                )
import           Data.Foldable                  ( Foldable(null) )
import           Data.Function                  (($) )
import           Data.Functor                   ( ($>)
                                                , void
                                                )
import           Data.Functor                   ( (<$>) )
import           Data.Generics.Product          ( field )
import           Data.Int                       ( Int )
import           Data.List                      ( (!!) )
import           Data.List                      ( intersperse )
import qualified Data.Map                      as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     (maybe,  Maybe(..) )
import           Data.Monoid                    ( Monoid(mempty) )
import           Data.Ord                       (Ord((>))
                                                )
import           Data.Semigroup                 ( Endo(..)
                                                , Semigroup((<>))
                                                )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text
                                                , toLower
                                                )
import           Data.Text                      ( length )
import           Data.Witherable                ( Filterable(filter) )
import           GHC.Enum                       ( Enum(succ)
                                                , pred
                                                )
import           GHC.Float                      ( Double )
import           GHC.Num                        (Num((+))
                                                )
import           GHC.Real                       ( (/)
                                                , fromIntegral
                                                )
import           Obelisk.Route.Frontend         ( R
                                                , RouteToUrl
                                                , SetRoute(setRoute)
                                                , routeLink
                                                , pattern (:/)
                                                )
import           Palantype.Common               ( kiChordsStart
                                                , Chord
                                                , Lang(..)
                                                , Palantype
                                                , PatternPos
                                                , unparts
                                                )
import           Palantype.Common               ( RawSteno
                                                , parseSteno
                                                )
import           Palantype.Common               ( kiBackUp
                                                , kiEnter
                                                )
import qualified Palantype.Common.Indices      as KI
import           Reflex.Dom                     ( TriggerEvent
                                                , Performable
                                                , PerformEvent
                                                , widgetHold
                                                , switchDyn
                                                , holdUniqDyn
                                                , (=:)
                                                , DomBuilder
                                                , EventName(Click)
                                                , EventWriter
                                                , HasDomEvent(domEvent)
                                                , MonadHold
                                                , PostBuild(getPostBuild)
                                                , Prerender
                                                , Reflex(Event, never)
                                                , blank
                                                , dyn_
                                                , el
                                                , elAttr
                                                , elClass
                                                , elClass'
                                                , foldDyn
                                                , leftmost
                                                , performEvent
                                                , text
                                                , updated
                                                )
import           Safe                           ( initMay )
import           Shared                         ( iFa
                                                , whenJust
                                                )
import           State                          (Stats (..), stStats,  Env(..)
                                                , Message(..)
                                                , Navigation(..)
                                                , State
                                                , stageUrl
                                                , updateState
                                                )
import           System.Random.Shuffle          ( shuffleM )
import           TextShow                       ( showt )
import           Text.Read                      ( read )
import           Control.Lens                   ( makePrisms
                                                , makeLenses
                                                )
import qualified Palantype.Common.RawSteno     as Raw
import           Data.Function                  ( (&) )
import           Data.Functor                   ( Functor(fmap) )
import           Data.Foldable                  ( Foldable(elem) )
import           Page.Common.Stopwatch
import Control.Monad (when)
import Data.Foldable (Foldable(minimum))
import Data.List (take)

elFooter
    :: forall t (m :: * -> *)
     . ( DomBuilder t m
       , Prerender t m
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       )
    => Lang
    -> Navigation
    -> m ()
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

elBackUp
    :: forall key (m :: * -> *) t . (DomBuilder t m, Palantype key) => m ()
elBackUp =
    elClass "span" "btnSteno" $ text $ "↤ " <> showt (KI.toRaw @key kiBackUp) -- U+21A4

data Congraz
  = CongrazShow (Maybe Stats)
  | CongrazHide

elCongraz
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
    => Event t (Maybe Stats)
    -> Navigation
    -> m ()
elCongraz evDone Navigation {..} = mdo
    eChord <- asks envEChord
    dynStats <- asks $ envDynState >>> fmap (stStats >>> Map.findWithDefault [] (navLang, navCurrent))

    let eChordEnter  = void $ filter (\c -> KI.fromChord c == kiEnter) eChord
        eChordBackUp = void $ filter (\c -> KI.fromChord c == kiBackUp) eChord

        evCongraz = leftmost [CongrazShow <$> evDone, eBack $> CongrazHide]

    updateState $ evDone <&> maybe [] \s ->
        [ field @"stStats" %~ Map.insertWith (<>) (navLang, navCurrent) [s] ]

    eBack <- fmap switchDyn $ widgetHold (pure never) $ evCongraz <&> \case
        CongrazHide    -> pure never
        CongrazShow mt -> elClass "div" "mkOverlay" $ elClass "div" "congraz" $ do
            el "div" $ text "Task cleared!"
            elClass "div" "check" $ iFa "fas fa-check-circle"
            whenJust mt $ \stats -> dyn_ $ dynStats <&> \ls -> do
                when (null ls || statsTime stats == minimum (statsTime <$> ls)) $
                        elClass "div" "paragraph newBest" $ do
                            iFa "fa-solid fa-star-sharp"
                            el "strong" $ text $ "New best: " <> formatTime (statsTime stats)
                            iFa "fa-solid fa-star-sharp"
                elStatistics $ take 3 ls
                elClass "hr" "visibilityHidden" blank
            whenJust navMNext $ \nxt -> do
                elACont <- elClass "div" "anthrazit" $ do
                    text "Type "
                    elClass "span" "btnSteno" $ do
                        el "em" $ text "Enter "
                        el "code" $ text $ showt $ KI.toRaw @key kiEnter
                    text " to continue to "
                    elClass "div" "paragraph" $ do
                        (e, _) <-
                            elClass' "a" "normalLink" $ text $ showt $ stageMeta
                                nxt
                        text "."
                        pure e
                let eContinue = leftmost [eChordEnter, domEvent Click elACont]
                updateState
                    $  eContinue
                    $> [ field @"stProgress"
                           %~ Map.update
                                  (\s -> if nxt > s then Just nxt else Just s)
                                  navLang
                       , field @"stCleared" %~ Set.insert navCurrent
                       , if nxt == read "stage_2-1"
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

parseStenoOrError
    :: forall proxy key t (m :: * -> *)
     . (EventWriter t (Endo State) m, Palantype key, PostBuild t m)
    => proxy key
    -> RawSteno
    -> m (Maybe [Chord key])
parseStenoOrError _ raw = case parseSteno raw of
    Right words -> pure $ Just words
    Left  err   -> do
        ePb <- getPostBuild
        let msgCaption = "Internal error"
            msgBody =
                "Could not parse steno code: " <> showt raw <> "\n" <> err
        updateState $ ePb $> [field @"stMsg" .~ Just Message { .. }]
        pure Nothing

elNotImplemented :: forall (m :: * -> *) t . DomBuilder t m => m ()
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

loading :: forall (m :: * -> *) t . (DomBuilder t m) => m ()
loading = elClass "div" "paragraph" $ do
    iFa "fas fa-spinner fa-spin"
    text " Loading ..."

data StateWords
    = StatePause
    | StateRun Run

data Run = Run
    { _stCounter   :: Int
    , _stChords    :: [RawSteno]
    , _stWords     :: [Text]
    , _stNMistakes :: Int
    , _stMHint     :: Maybe [RawSteno]
    }

makePrisms ''StateWords
makeLenses ''Run

taskWords
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , MonadFix m
       , MonadHold t m
       , MonadIO (Performable m)
       , MonadReader (Env t key) m
       , Palantype key
       , PerformEvent t m
       , PostBuild t m
       , Prerender t m
       , TriggerEvent t m
       )
    => Map RawSteno Text
    -> Map Text [RawSteno]
    -> m (Event t Stats)
taskWords mapStenoWord mapWordStenos = do
    eChord   <- asks envEChord

    evStdGen <- postRender $ do
        ePb <- getPostBuild
        performEvent $ ePb $> liftIO newStdGen

    fmap switchDyn $ widgetHold (loading $> never) $ evStdGen <&> \stdGen -> do
        let len = Map.size mapWordStenos

            step :: Chord key -> StateWords -> StateWords
            step c st = case st of
                StatePause ->
                    if Raw.fromChord c `elem` (KI.toRaw @key <$> kiChordsStart)
                        then stepStart
                        else st
                -- undo last input
                StateRun Run {..}
                    | Raw.fromChord c == KI.toRaw @key kiBackUp ->
                      case initMay _stChords of
                          Just cs ->
                              st &  _StateRun %~
                                   (stChords .~ cs)
                                 . (stNMistakes %~ succ)
                                 . (stMHint .~ Nothing)
                          Nothing -> st & _StateRun . stMHint ?~
                              ( Map.findWithDefault
                                  []
                                  (_stWords !! _stCounter)
                                  mapWordStenos
                              )
                StateRun Run {..} ->
                    let rawSteno = unparts $ _stChords <> [Raw.fromChord c]
                        word     = Map.findWithDefault "" rawSteno mapStenoWord
                    in  if word == _stWords !! _stCounter

                            -- correct
                            then if _stCounter == pred len
                                then StatePause
                                else
                                    st & _StateRun %~
                                      (stCounter %~ succ)
                                    . (stMHint .~ Nothing)
                                    . (stChords .~ [])

                            -- not correct
                            else st & _StateRun . stChords .~
                                    (_stChords <> [Raw.fromChord c])
            stepStart = StateRun Run
                { _stCounter   = 0
                , _stChords    = []
                , _stWords     = evalRand
                                     (shuffleM $ Map.keys mapWordStenos)
                                     stdGen
                , _stNMistakes = 0
                , _stMHint     = Nothing
                }

            stateInitial = StatePause

        dynStateWords <- foldDyn step stateInitial eChord

        evStartStop <-
            fmap updated $ holdUniqDyn $ dynStateWords <&> \case
                StatePause -> False
                StateRun _ -> True
        dynStopwatch <- mkStopwatch evStartStop

        elClass "div" "taskWords" $ do
            dyn_ $ dynStateWords <&> \case
                StatePause -> el "div" $ do
                    text "Type "
                    elClass "span" "btnSteno blinking" $ do
                        text "Start "
                        el "code" $ text "SDAÜD"
                    text " to begin the exercise."
                StateRun Run {..} -> do
                    -- TODO: what is span ".word"?
                    elClass "span" "word"
                        $  elClass "span" "exerciseField"
                        $  el "code"
                        $  text
                        $  _stWords
                        !! _stCounter

                    elClass "span" "input" $ traverse_ (el "code" <<< text) $
                        intersperse "/" $ (showt <$> _stChords) <> [" …"]

                    el "span" $ do
                        elClass "span" "btnSteno" $ text $ "↤ " <> showt
                            (KI.toRaw @key kiBackUp) -- U+21A4
                        elClass "span" "small" $ text $ if null _stChords
                            then " to show hint"
                            else " to back up"

                    whenJust _stMHint $ \hint ->
                        elClass "span" "small" $ for_ hint $ \r -> do
                            text $ showt r
                            el "br" blank

                    elClass "hr" "visibilityHidden" blank
                    el "strong" $ text $ showt _stCounter
                    text $ " / " <> showt len

            elStopwatch dynStopwatch len

elPatterns
    :: forall (m :: * -> *) t
     . DomBuilder t m
    => [(PatternPos, [(Text, RawSteno)])]
    -> m ()
elPatterns doc = elClass "div" "patternTable" $ traverse_ elPatterns' doc
  where
    elPatterns' (pPos, pairs) = do
        let strPPos = toLower $ showt pPos
        elClass "hr" strPPos blank
        elClass "span" ("patternPosition " <> strPPos) $ text strPPos
        elClass "br" "clearBoth" blank
        for_ pairs $ \(orig, steno) -> elClass "div" "floatLeft" $ do
            let lOrig :: Double = fromIntegral $ length orig
                styleOrig       = if lOrig > 6
                    then
                        "style"
                            =: (  "font-size: "
                               <> showt ((1 + 6 / lOrig) / 2)
                               <> "em"
                               )
                    else mempty
                lSteno :: Double = fromIntegral $ length $ showt steno
                styleSteno       = if lSteno > 6
                    then
                        "style" =: ("font-size: " <> showt (6 / lSteno) <> "em")
                    else mempty
            elAttr "div" ("class" =: "orig" <> styleOrig) $ text orig
            elAttr "code" ("class" =: "steno" <> styleSteno) $ text $ showt
                steno
        elClass "br" "clearBoth" blank
