{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
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

import           Client                         (getStats, getMaybeAuthData, postEventStageCompleted, request,  postRender )
import           Common.Route                   ( FrontendRoute(..) )
import           Common.Stage                   (StageMeta(StageTopLevel, StageSubLevel), Stage,  stageMeta )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( (<<<)
                                                , Category((.))
                                                )
import           Control.Lens                   ( (?~)
                                                , (%~)
                                                , (.~)
                                                , preview
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Random           ( evalRand
                                                , newStdGen
                                                )
import           Control.Monad.Reader           (ask,  MonadReader

                                                )
import           Data.Bool                      (not,  (||)
                                                , Bool(..)
                                                )
import           Data.Either                    ( Either(..) )
import           Data.Eq                        ( Eq((==)) )
import           Data.Foldable                  ( for_
                                                , traverse_
                                                )
import           Data.Foldable                  ( Foldable(null) )
import           Data.Function                  (flip, const,  ($) )
import           Data.Functor                   ( ($>)
                                                , void
                                                , (<$>)
                                                , (<&>)
                                                )
import           Data.Generics.Product          ( field )
import           Data.Generics.Sum          ( _As )
import           Data.Int                       ( Int )
import           Data.List                      (sortOn, (!!)
                                                , head
                                                )
import           Data.List                      ( intersperse )
import qualified Data.Map                      as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     (fromMaybe, maybe, isJust
                                                , Maybe(..)
                                                )
import           Data.Monoid                    ( Monoid(mempty) )
import           Data.Ord                       (Down  (Down),  Ord((>)) )
import           Data.Semigroup                 ( Endo(..)
                                                , Semigroup((<>))
                                                )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import           Data.Text                      ( length )
import           Data.Witherable                ( Filterable(filter) )
import           GHC.Enum                       ( Enum(succ)
                                                , pred
                                                )
import           GHC.Float                      ( Double )
import           GHC.Num                        ( Num((+)) )
import           GHC.Real                       ( (/)
                                                , fromIntegral
                                                )
import           Obelisk.Route.Frontend         (Routed, askRoute,  R
                                                , RouteToUrl
                                                , SetRoute(setRoute)
                                                , routeLink
                                                )
import           Palantype.Common               ( kiChordsStart
                                                , Chord
                                                , Lang(..)
                                                , Palantype
                                                , PatternPos
                                                , unparts
                                                , showPretty
                                                )
import           Palantype.Common               ( RawSteno
                                                , parseSteno
                                                )
import           Palantype.Common               ( kiBackUp
                                                , kiEnter
                                                )
import qualified Palantype.Common.Indices      as KI
import           Reflex.Dom                     (elAttr', performEvent_, prerender_, gate, attach, current, fanEither, prerender, constDyn, Dynamic,  TriggerEvent
                                                , Performable
                                                , PerformEvent
                                                , widgetHold
                                                , switchDyn
                                                , holdUniqDyn
                                                , (=:)
                                                , DomBuilder
                                                , Element (_element_raw)
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
import           Shared                         (dynSimple, formatTime,  iFa
                                                , whenJust
                                                )
import           State                          (Session (..),  State(..)
                                                , Env(..)
                                                , Navigation(..)
                                                , State
                                                , stageUrl
                                                , updateState
                                                )
import           System.Random.Shuffle          ( shuffleM )
import           TextShow                       ( showt )
import qualified Palantype.Common.RawSteno     as Raw
import           Data.Function                  ( (&) )
import           Data.Functor                   ( Functor(fmap) )
import           Data.Foldable                  ( Foldable(elem) )
import           Page.Common.Stopwatch          (elStatisticsPersonalShort,  elStopwatch
                                                , mkStopwatch


                                                )
import           Control.Monad                  ( when )
import           Data.Foldable                  ( Foldable(minimum) )
import           Data.List                      ( take )
import           Common.Model                   (AppState (stSound),  Message(..)
                                                , Stats (..)

                                                )
import Data.Witherable (Filterable(catMaybes))
import qualified LocalStorage as LS
import Language.Javascript.JSaddle (liftJSM)
import Data.Bifunctor (Bifunctor(second))
import Obelisk.Generated.Static (static)
import GHC.Generics (Generic)
import GHCJS.DOM.Types (HTMLAudioElement(HTMLAudioElement), unElement)
import GHCJS.DOM.HTMLMediaElement (play)

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

elCongraz
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
    => Event t (Maybe Stats)
    -- list of personal stats
    -> Dynamic t [Stats]
    -> Navigation
    -> m (Dynamic t Bool)
elCongraz evDone dynStats Navigation {..} = mdo
    Env {..} <- ask

    let
        eChordEnter  = void $ filter (\c -> KI.fromChord c == kiEnter) envEChord
        eChordBackUp = void $ filter (\c -> KI.fromChord c == kiBackUp) envEChord

    dynDone <- foldDyn const Nothing $ leftmost [Just <$> evDone, evRepeat $> Nothing]
    let evNewStats = catMaybes $ catMaybes $ updated dynDone

    _ <- request $ postEventStageCompleted
      (getMaybeAuthData <$> envDynState)
      (dynDone <&> maybe (Left "not ready")
        (maybe (Left "no stats") \stats ->
          Right (navLang, navCurrent, stats)
        )
      )
      (void evNewStats)

    let isLoggedIn = \case
          SessionAnon   -> False
          SessionUser _ -> True
        evLSPutStats =
          gate (not . isLoggedIn . stSession <$> current envDynState) evNewStats

    dynStage <- askRoute
    prerender_ blank $ performEvent_ $ attach (current dynStage) evLSPutStats <&> \(stage, stats) -> do
      mMap <- liftJSM $ LS.retrieve LS.KeyStats
      let oldStats = fromMaybe Map.empty mMap
      liftJSM $ LS.put LS.KeyStats $ Map.insertWith (<>) (navLang, stage) [stats] oldStats

    evRepeat <- dynSimple $ dynDone <&> \case
        Nothing -> pure never
        Just mNewStats ->
            elClass "div" "mkOverlay" $ elClass "div" "congraz" $ do
                el "div" $ text "Task cleared!"
                elClass "div" "check" $ iFa "fas fa-check-circle"
                whenJust mNewStats $ \newStats -> dyn_ $ dynStats <&> \lsStats -> do
                    -- let lsStatsPersonal = filter (isNothing . fst . snd) lsStats
                    when (null lsStats || statsTime newStats == minimum (statsTime <$> lsStats))
                        $ elClass "div" "paragraph newBest" $ do
                              iFa "fa-solid fa-star-sharp"
                              el "strong" $ text $ "New personal best: " <> formatTime
                                  (statsTime newStats)
                              iFa "fa-solid fa-star-sharp"
                    elStatisticsPersonalShort lsStats
                    elClass "hr" "visibilityHidden" blank
                whenJust navMNext $ \nxt -> do
                    let nxtMeta = stageMeta nxt
                    elACont <- elClass "div" "anthrazit" $ do
                        text "Type "
                        elClass "span" "btnSteno" $ do
                            el "em" $ text "Enter "
                            el "code" $ text $ showt $ KI.toRaw @key kiEnter
                        text " to continue to "
                        elClass "div" "paragraph" $ do
                            (e, _) <- elClass' "a" "normalLink" $ text $ showt nxtMeta
                            text "."
                            pure e
                    let eContinue =
                            leftmost [eChordEnter, domEvent Click elACont]

                    updateState $ eContinue $>
                      [ field @"stApp" .  field @"stProgress" %~ Map.update
                                  (\s -> if nxt > s then Just nxt else Just s)
                                  navLang
                      , field @"stApp" .  field @"stCleared" %~ Set.insert navCurrent
                      ] <> case nxtMeta of
                          StageTopLevel _ -> []
                          StageSubLevel i _ _ ->
                            [ field @"stApp" . field @"stTOCShowStage" .~ Set.singleton i
                            ]

                    setRoute $ eContinue $> stageUrl navLang nxt

                el "div" $ do
                    el "span" $ text "("
                    (elABack, _) <- elClass' "a" "normalLink" $ text "back"
                    text " "
                    elBackUp @key
                    el "span" $ text ")"
                    pure $ leftmost [eChordBackUp, domEvent Click elABack]
    pure $ isJust <$> dynDone

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
        updateState
            $  ePb
            $> [field @"stApp" . field @"stMsg" .~ Just Message { .. }]
        pure Nothing

elNotImplemented :: forall (m :: * -> *) t . DomBuilder t m => m ()
elNotImplemented = elClass "blockquote" "warning" $ do
    el "strong" $ text "Not implemented"
    el "br" blank
    text
        "You are currently looking at an exercise that has not been \
        \implemented for the original English palantype. \
        \Feel free to read, but don't expect things to work from here on."

loading :: forall (m :: * -> *) t . (DomBuilder t m) => m ()
loading = elClass "div" "paragraph" $ do
    iFa "fas fa-spinner fa-spin"
    text " Loading ..."

data StateWords
    = StatePause Int
    | StateRun Run
    deriving (Generic)

data Run = Run
    { stCounter   :: Int
    , stChords    :: [RawSteno]
    , stWords     :: [Text]
    , stNMistakes :: Int
    , stMHint     :: Maybe [RawSteno]
    } deriving (Generic)

taskWords
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
       , TriggerEvent t m
       )
    => Dynamic t [(Bool, (Maybe Text, Stats))]
    -> Event t (Chord key)
    -> Map RawSteno Text
    -> Map Text [RawSteno]
    -> m (Event t Stats)
taskWords dynStats evChord mapStenoWord mapWordStenos = do

    evStdGen <- postRender $ do
        ePb <- getPostBuild
        performEvent $ ePb $> liftIO newStdGen


    fmap switchDyn $ widgetHold (loading $> never) $ evStdGen <&> \stdGen -> do

        let len = Map.size mapWordStenos
            step :: Chord key -> StateWords -> StateWords
            step c st = case st of
                StatePause _ ->
                    if Raw.fromChord c `elem` (KI.toRaw @key <$> kiChordsStart)
                        then stepStart
                        else st
                -- undo last input
                StateRun Run {..} | Raw.fromChord c == KI.toRaw @key kiBackUp ->
                    case initMay stChords of
                        Just cs ->
                            st
                                &  _As @"StateRun"
                                %~ (field @"stChords" .~ cs)
                                .  (field @"stNMistakes" %~ succ)
                                .  (field @"stMHint" .~ Nothing)
                        Nothing ->
                            st
                                &  _As @"StateRun"
                                .  field @"stMHint"
                                ?~ (Map.findWithDefault
                                       []
                                       (stWords !! stCounter)
                                       mapWordStenos
                                   )
                StateRun Run {..} ->
                    let rawSteno = unparts $ stChords <> [Raw.fromChord c]
                        word     = Map.findWithDefault "" rawSteno mapStenoWord
                    in  if word == stWords !! stCounter

                            -- correct
                            then if stCounter == pred len
                                then StatePause stNMistakes
                                else
                                    st
                                    &  _As @"StateRun"
                                    %~ (field @"stCounter" %~ succ)
                                    .  (field @"stMHint" .~ Nothing)
                                    .  (field @"stChords" .~ [])

                            -- not correct
                            else
                                st
                                &  _As @"StateRun"
                                .  field @"stChords"
                                .~ (stChords <> [Raw.fromChord c])
            stepStart = StateRun Run
                { stCounter   = 0
                , stChords    = []
                , stWords = evalRand (shuffleM $ Map.keys mapWordStenos) stdGen
                , stNMistakes = 0
                , stMHint     = Nothing
                }

            stateInitial = StatePause 0

        dynStateWords <- foldDyn step stateInitial evChord

        evStartStop   <- fmap updated $ holdUniqDyn $ dynStateWords <&> \case
            StatePause nMistakes -> nMistakes
            StateRun   _         -> -1
        dynStopwatch <- mkStopwatch evStartStop

        elClass "div" "taskWords" $ do

            evTrigger <- void . updated <$> holdUniqDyn
              ( dynStateWords <&> fromMaybe 0
                  . preview (_As @"StateRun" . field @"stCounter")
              )
            elBtnSound evTrigger

            dyn_ $ dynStateWords <&> \case
                StatePause _ -> el "div" $ do
                    text "Type "
                    elClass "span" "btnSteno blinking" $ do
                        text "Start "
                        el "code" $ text $ showt $ KI.toRaw @key $ head
                            kiChordsStart
                    text " to begin the exercise."
                StateRun Run {..} -> do
                    -- TODO: what is span ".word"?
                    elClass "span" "word"
                        $  elClass "span" "exerciseField"
                        $  el "code"
                        $  text
                        $  stWords
                        !! stCounter

                    elClass "span" "input"
                        $  traverse_ (el "code" <<< text)
                        $  intersperse "/"
                        $  (showt <$> stChords)
                        <> [" …"]

                    el "span" $ do
                        elClass "span" "btnSteno" $ text $ "↤ " <> showt
                            (KI.toRaw @key kiBackUp) -- U+21A4
                        elClass "span" "small" $ text $ if null stChords
                            then " to show hint"
                            else " to back up"

                    whenJust stMHint $ \hint ->
                        elClass "span" "small" $ for_ hint $ \r -> do
                            text $ showt r
                            el "br" blank

                    elClass "hr" "visibilityHidden" blank
                    el "strong" $ text $ showt stCounter
                    text $ " / " <> showt len

            elStopwatch dynStats dynStopwatch len

elPatterns
    :: forall (m :: * -> *) t
     . DomBuilder t m
    => [(PatternPos, [(Text, RawSteno)])]
    -> m ()
elPatterns doc = elClass "div" "patternTable" $ traverse_ elPatterns' doc
  where
    elPatterns' (pPos, pairs) = do
        elClass "hr" (showt pPos) blank
        elClass "span" ("patternPosition " <> showt pPos) $ text $ showPretty
            pPos
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

-- | get the statistics for the score board for the current page
--   'evNewStats': an event stream of new stats records
--   evaluates to a dynamic of a list of tuples:
--       Bool: whether or not this is a public stats record (visible to anyone)
--       Maybe Text: Nothing for personal stats, Just aliasName for the stats of others
getStatsLocalAndRemote
  :: forall key (m :: * -> *) t
  . ( EventWriter t (Endo State) m
    , MonadFix m
    , MonadHold t m
    , MonadReader (Env t key) m
    , PerformEvent t m
    , PostBuild t m
    , Prerender t m
    , Routed t Stage m
    )
  => Event t Stats
  -> m (Dynamic t [(Bool, (Maybe Text, Stats))])
getStatsLocalAndRemote evNewStats = do
  Env{..} <- ask
  let Navigation{..} = envNavigation
  evReady <- fmap envToReady $ getPostBuild
  dynStage <- askRoute

  (evRespFail, evRespSucc) <- fmap fanEither $ request $
      getStats (getMaybeAuthData <$> envDynState)
               (constDyn $ Right navLang)
               (Right <$> dynStage)
               evReady

  updateState $ evRespFail $> [ field @"stApp" . field @"stMsg" ?~
                                  Message "Error" "Couldn't load statistics"
                              ]

  let dynMIsVisible =
        envDynState <&> preview (field @"stSession" . _As @"SessionUser" . field @"sdAliasVisible")
      evNewStats' = attach (current dynMIsVisible <&> fromMaybe False) $ evNewStats <&> \s -> (Nothing, s)

  -- TODO: performEvent within prerender doesn't seem to work
  --let isLoggedIn = \case
  --      SessionAnon   -> False
  --      SessionUser _ -> True
  --    evAnon =
  --      tag (current dynStage) $
  --      filter id $
  --      tagPromptlyDyn (not . isLoggedIn . stSession <$> envDynState) evReady

  --evLSMapStats <- fmap switchDyn $ prerender (pure never) $ performEvent $ evAnon <&> \stage -> do
  --  mMap <- liftJSM $ LS.retrieve LS.KeyStats
  --  pure $ Map.findWithDefault [] (navLang, stage) $ fmap (Nothing,) <$> fromMaybe Map.empty mMap

  --       this is a workaround
  --       .. and I don't know why this works, `updated <$> prerender` fires
  --       also when I navigate through the pages
  evLSMapStats <- fmap updated $ prerender (pure Map.empty) $
    fromMaybe Map.empty <$> liftJSM (LS.retrieve LS.KeyStats)

  let evLSStats = attach (current dynStage) evLSMapStats <&> \(stage, map) ->
        (False, ) . (Nothing, ) <$> Map.findWithDefault [] (navLang, stage) map

  fmap (fmap $ sortOn (Down . second (second statsDate)) . take 10) $ foldDyn (flip (<>)) [] $ leftmost
    [ evLSStats
    , fmap (True,) <$> evRespSucc
    , pure <$> evNewStats'
    ]

elBtnSound
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadReader (Env t key) m
       , PostBuild t m
       , Prerender t m
       )
    => Event t ()
    -> m ()
elBtnSound evTrigger = do

    Env{..} <- ask
    let dynSound = stSound . stApp <$> envDynState
        evPlaySound = gate (current dynSound) evTrigger

    prerender_ blank do
      (domAudio, _) <- elAttr' "audio"
         (  "src"     =: $(static "tick.mp3")
         <> "preload" =: "auto"
         <> "id"      =: "audio-tick"
         ) blank
      let audioEl = HTMLAudioElement $ unElement $ _element_raw domAudio
      performEvent_ $ evPlaySound $> play audioEl

    (domSound, _) <- elAttr' "span"
      (  "class" =: "floatRight icon-link"
      <> "title" =: "toggle sound"
      ) $ dyn_ $ dynSound <&> \case
        True  -> iFa "fas fa-volume-down"
        False -> iFa "fas fa-volume-mute"

    updateState $ domEvent Click domSound $> [field @"stApp" . field @"stSound" %~ not]
