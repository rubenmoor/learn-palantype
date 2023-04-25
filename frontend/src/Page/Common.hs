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

import           Client                         ( getMaybeAuthData
                                                , getStats
                                                , postEventStageCompleted
                                                , postRender
                                                , request
                                                )
import           Common.Model                   ( AppState(stSound)
                                                , Message(..)
                                                , Stats(..)
                                                )
import           Common.Route                   ( FrontendRoute(..) )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( (<<<)
                                                , Category((.))
                                                )
import           Control.Lens                   ( (%~)
                                                , (.~)
                                                , (?~)
                                                , preview
                                                )
import           Control.Monad                  ( when
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Random           ( evalRand
                                                , newStdGen
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , ask
                                                )
import           Data.Bifunctor                 ( Bifunctor(second) )
import           Data.Bool                      ( Bool(..)
                                                , not
                                                , (||), (&&)
                                                )
import           Data.Either                    ( Either(..) )
import           Data.Eq                        ( Eq((==)) )
import           Data.Foldable                  ( Foldable(minimum, null)
                                                , for_
                                                , traverse_
                                                )
import           Data.Function                  ( ($)
                                                , (&)
                                                , const
                                                , flip
                                                )
import           Data.Functor                   ( ($>)
                                                , (<$>)
                                                , (<&>)
                                                , Functor(fmap)
                                                , void
                                                )
import           Data.Generics.Product          ( field )
import           Data.Generics.Sum              ( _As )
import           Data.Int                       ( Int )
import           Data.List                      ( (!!)
                                                , intersperse
                                                , sortOn
                                                , take
                                                )
import qualified Data.Map                      as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( Maybe(..)
                                                , fromMaybe
                                                , isJust
                                                , maybe, isNothing
                                                )
import           Data.Monoid                    ( Monoid(mempty) )
import           Data.Ord                       ( Down(Down)
                                                , Ord((>))
                                                )
import           Data.Semigroup                 ( Endo(..)
                                                , Semigroup((<>))
                                                )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text
                                                , length
                                                )
import           GHC.Enum                       ( Enum(succ)
                                                , pred
                                                )
import           GHC.Float                      ( Double )
import           GHC.Generics                   ( Generic )
import           GHC.Num                        ( Num((+)) )
import           GHC.Real                       ( (/)
                                                , fromIntegral
                                                )
import           GHCJS.DOM.HTMLMediaElement     ( play )
import           GHCJS.DOM.Types                ( HTMLAudioElement
                                                    ( HTMLAudioElement
                                                    )
                                                , unElement
                                                )
import           Language.Javascript.JSaddle    ( liftJSM )
import qualified LocalStorage                  as LS
import           Obelisk.Generated.Static       ( static )
import           Obelisk.Route.Frontend         ( R
                                                , RouteToUrl
                                                , SetRoute
                                                )
import           Page.Common.Stopwatch          ( elStatisticsPersonalShort
                                                , elStopwatch
                                                , mkStopwatch
                                                )
import           Palantype.Common               ( Chord
                                                , Palantype
                                                , PatternPos (..)
                                                , RawSteno
                                                , kiBackUp
                                                , kiEnter
                                                , showPretty
                                                , unparts, SystemLang (..), parseChordMaybe, getSystemLang, kiLeft, kiRight
                                                )
import qualified Palantype.Common.Indices      as KI
import qualified Palantype.Common.RawSteno     as Raw
import qualified Palantype.Common.Stage        as Stage
import           Reflex.Dom                     ( (=:)
                                                , DomBuilder
                                                , Dynamic
                                                , Element(_element_raw)
                                                , EventName(Click)
                                                , EventWriter
                                                , HasDomEvent(domEvent)
                                                , MonadHold
                                                , PerformEvent
                                                , Performable
                                                , PostBuild(getPostBuild)
                                                , Prerender
                                                , Reflex(Event, never)
                                                , TriggerEvent
                                                , attach
                                                , blank
                                                , constDyn
                                                , current
                                                , dyn_
                                                , el
                                                , elAttr
                                                , elAttr'
                                                , elClass

                                                , fanEither
                                                , foldDyn
                                                , gate
                                                , holdUniqDyn
                                                , leftmost
                                                , performEvent
                                                , performEvent_
                                                , prerender
                                                , prerender_
                                                , switchDyn
                                                , text
                                                , updated
                                                , widgetHold, el'
                                                )
import           Safe                           ( initMay )
import           Shared                         ( dynSimple
                                                , formatTime
                                                , iFa
                                                , whenJust, elRouteLink, setRouteAndLoading
                                                )
import           State                          ( Env(..)
                                                , Navigation(..)
                                                , Session(..)
                                                , State(..)
                                                , stageUrl
                                                , updateState
                                                )
import           System.Random.Shuffle          ( shuffleM )
import           TextShow                       ( showt )
import           Witherable                     ( Filterable(catMaybes, filter)
                                                )
import Palantype.Common.TH (fromJust)

elFooter
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , Palantype key
       , Prerender t m
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       )
    => Navigation
    -> m ()
elFooter Navigation {..} =
  elClass "footer" "grow-0 shrink shadow-[0_-4px_6px_-1px_rgba(0,0,0,0.1)] text-xl p-3 \
                   \text-center z-10" do
    whenJust navMPrevious $ \prv ->
      elClass "div" "float-left" do
        text "< "
        elRouteLink (stageUrl @key prv) $
          text $ maybe "" Stage.showShort $ Stage.fromIndex @key prv
        elClass "span" "steno-navigation ml-2 p-2" $ text
          $ "⯇ " <> showt (KI.toRaw @key kiLeft)

    text $ maybe "" Stage.showShort $ Stage.fromIndex @key navCurrent

    whenJust navMNext $ \nxt ->
      elClass "div" "float-right" do
        elClass "span" "steno-navigation mr-2 p-2" $ text
          $ "⯈ " <> showt (KI.toRaw @key kiRight)
        elRouteLink (stageUrl @key nxt) $
          text $ maybe "" Stage.showShort $ Stage.fromIndex @key nxt
        text " >"
    elClass "br" "clear-both" blank

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
        eChordEnter = void $ filter (\c -> fmap KI.fromChord c == Just kiEnter ) envEvMChord

    dynDone <- foldDyn const Nothing $ leftmost [Just <$> evDone, evRepeat $> Nothing]
    let evNewStats = catMaybes $ catMaybes $ updated dynDone

    dynAuthData <- holdUniqDyn $ getMaybeAuthData <$> envDynState
    _ <- request $ postEventStageCompleted
      dynAuthData
      (dynDone <&> maybe (Left "not ready")
        (maybe (Left "no stats") \stats ->
          Right (navSystemLang, navCurrent, stats)
        )
      )
      (void evNewStats)

    let isLoggedIn = \case
          SessionAnon   -> False
          SessionUser _ -> True
        evLSPutStats =
          gate (not . isLoggedIn . stSession <$> current envDynState) evNewStats

    prerender_ blank $ performEvent_ $ evLSPutStats <&> \stats ->
      LS.update LS.KeyStats $
              Map.insertWith (<>) (navSystemLang, navCurrent) [stats]
          <<< fromMaybe Map.empty

    evRepeat <- dynSimple $ dynDone <&> \case
        Nothing -> pure never
        Just mNewStats ->

            elClass "div" "overlay" $ elClass "div" "text-center text-md" $ do
                elClass "div" "text-4xl" $ text "Task cleared!"
                elClass "div" "text-[72pt] text-green-500" $ iFa "fas fa-check-circle"
                whenJust mNewStats $ \newStats -> dyn_ $ dynStats <&> \lsStats -> do
                    when (null lsStats || statsTime newStats == minimum (statsTime <$> lsStats)) do
                      elClass "p" "text-shadow" $ do
                              elClass "span" "px-2 text-yellow-500" $ iFa "fas fa-star"
                              el "strong" $ text $ "New personal best: " <> formatTime
                                  (statsTime newStats)
                              elClass "span" "px-2 text-yellow-500" $ iFa "fas fa-star"
                      el "br" blank

                    elStatisticsPersonalShort lsStats
                    el "br" blank

                elClass "div" "text-grayishblue-900" $ do
                  whenJust navMNext \nxt -> do
                    text "Type "
                    elClass "span" "steno-action" $ do
                        text "Enter "
                        el "code" $ text $ showt $ KI.toRaw @key kiEnter
                    text " to continue to "
                    (domNextStage, _) <- el' "a" $ text $ maybe "" Stage.showShort
                        $ Stage.fromIndex @key nxt
                    text " or "

                    let eContinue = leftmost [eChordEnter, domEvent Click domNextStage]

                    updateState $ eContinue $>
                        [ field @"stApp" . field @"stToc" . field @"stProgress" %~ Map.update
                                    (\s -> if nxt > s then Just nxt else Just s)
                                    navSystemLang
                        , field @"stApp" . field @"stToc" . field @"stCleared" %~ Set.insert navCurrent
                        ] <> case Stage.fromIndex @key nxt of
                            Nothing -> []
                            Just  t ->
                              [   field @"stApp" . field @"stToc"
                                . field @"stShowStage" .~ Set.singleton (Stage.getGroupIndex t)
                              ]

                    setRouteAndLoading $ eContinue $> stageUrl @key nxt

                  text "go "
                  (domABack, _) <- el' "a" $ text "back"
                  text " "
                  elClass "span" "steno-navigation p-1" $
                      text $ "↤ " <> showt (KI.toRaw @key kiBackUp) -- U+21A4
                  text " to repeat the exercise."
                  pure $ leftmost [void $ filter isNothing envEvMChord, domEvent Click domABack]
    pure $ isJust <$> dynDone

chordStart :: forall key. Palantype key => Chord key
chordStart = $fromJust $ parseChordMaybe @key $ case getSystemLang @key of
  SystemDE -> "DSAÜD"
  SystemEN -> "START"

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
    -> Event t (Maybe (Chord key))
    -> Map RawSteno Text
    -> Map Text [RawSteno]
    -> m (Event t Stats)
taskWords dynStats evChord mapStenoWord mapWordStenos = do

    evStdGen <- postRender $ do
        ePb <- getPostBuild
        performEvent $ ePb $> liftIO newStdGen

    fmap switchDyn $ widgetHold (loading $> never) $ evStdGen <&> \stdGen -> do

        let len = Map.size mapWordStenos
            step :: Maybe (Chord key) -> StateWords -> StateWords
            step mc st = case (st, mc) of
                (StatePause _, Just c) | c == chordStart @key -> stepStart
                (StatePause _, _) -> st
                -- undo last input
                (StateRun Run {..}, Nothing) ->
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
                                ?~ Map.findWithDefault
                                       []
                                       (stWords !! stCounter)
                                       mapWordStenos
                (StateRun Run {..}, Just c) ->
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
                                .~ stChords <> [Raw.fromChord c]
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

        elClass "div" "mt-8 text-lg" $ do

            evTrigger <- void . updated <$> holdUniqDyn
              ( dynStateWords <&> fromMaybe 0
                  . preview (_As @"StateRun" . field @"stCounter")
              )
            elBtnSound evTrigger

            dyn_ $ dynStateWords <&> \case
                StatePause _ -> el "div" $ do
                    text "Type "
                    elClass "span" "steno-action" $ do
                        text "Start "
                        el "code" $ text $ showt $ chordStart @key
                    text " to begin the exercise."
                StateRun Run {..} -> do
                    elClass "span" "bg-zinc-200 rounded w-fit p-1 mx-2"
                        $  el "code"
                        $  text
                        $  stWords
                        !! stCounter

                    elClass "span" "mx-2"
                        $  traverse_ (el "code" <<< text)
                        $  intersperse "/"
                        $  (showt <$> stChords)
                        <> ["…"]

                    el "span" do
                        elClass "span" "steno-navigation p-1" $ text $ "↤ " <> showt
                            (KI.toRaw @key kiBackUp) -- U+21A4
                        elClass "span" "text-sm" $ text
                          $ if null stChords && isNothing stMHint
                            then " to show hint"
                            else " to back up"

                    whenJust stMHint $ \hint ->
                        elClass "span" "text-sm" $ for_ hint $ \r -> do
                            text "hint: "
                            elClass "code" "text-md" $ text $ showt r

                    el "br" blank
                    el "br" blank
                    el "strong" $ text $ showt stCounter
                    text $ " / " <> showt len

            elStopwatch dynStats dynStopwatch len

elPatterns
    :: forall (m :: * -> *) t
     . DomBuilder t m
    => [(PatternPos, [(Text, RawSteno)])]
    -> m ()
elPatterns doc = elClass "div" "my-4" $ traverse_ elPatterns' doc
  where
    elPatterns' (pPos, pairs) = do
        elDummy
        elClass "hr" ("my-2 border-none h-[1px] bg-" <> strColor pPos) blank
        elClass "span" ("float-right text-sm font-bold relative \
                        \text-" <> strColor pPos
                       ) $ text $ showPretty pPos
        for_ pairs $ \(orig, steno) -> elClass "div" "float-left" $ do
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
            elAttr "div" ("class" =: "bg-zinc-200 pr-2 w-24 h-8 text-right text-xl \
                                     \border border-white inline-block" <> styleOrig
                         ) $ text orig
            elAttr "code" ("class" =: "w-24 pl-1 text-left inline-block text-lg"
                           <> styleSteno
                          ) $ text $ showt
                steno
        elClass "br" "clear-both" blank

    strColor = \case
      Onset        -> "rose-400"
      Nucleus      -> "green-400"
      Coda         -> "blue-400"
      Multiple     -> "violet-400"
      OnsetAndCoda -> "orange-400"
      PPException  -> "grayishblue-900"

    -- make sure tailwind classes show up explicitly somewhere
    elDummy = elClass "div" "hidden" do
      elClass "span" "text-rose-400 bg-rose-400" blank
      elClass "span" "text-green-400 bg-green-400" blank
      elClass "span" "text-blue-400 bg-blue-400" blank
      elClass "span" "text-violet-400 bg-violet-400" blank
      elClass "span" "text-orange-400 bg-orange-400" blank
      elClass "span" "text-grayishblue-900 bg-grayishblue-900" blank

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
       , MonadIO (Performable m)
       , TriggerEvent t m
       )
    => Event t Stats
    -> m (Dynamic t [(Bool, (Maybe Text, Stats))])
getStatsLocalAndRemote evNewStats = do
  Env {..} <- ask
  let Navigation{..} = envNavigation

  evLoadedAndBuilt <- envGetLoadedAndBuilt
  (evRespFail, evRespSucc) <- fmap fanEither $ request $
      getStats (getMaybeAuthData <$> envDynState)
               (constDyn $ Right navSystemLang)
               (constDyn $ Right navCurrent)
               evLoadedAndBuilt

  updateState $ evRespFail $> [ field @"stApp" . field @"stMsg" ?~
                                  Message "Error" "Couldn't load statistics"
                              ]

  let dynMIsVisible =
        envDynState <&> preview (field @"stSession" . _As @"SessionUser" . field @"sdAliasVisible")
      evNewStats' = attach (current dynMIsVisible <&> fromMaybe False) $ (Nothing, ) <$> evNewStats

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
  --  pure $ Map.findWithDefault [] (navSystemLang, stage) $ fmap (Nothing,) <$> fromMaybe Map.empty mMap

  --       this is a workaround
  --       .. and I don't know why this works, `updated <$> prerender` fires
  --       also when I navigate through the pages
  evLSMapStats <- fmap updated $ prerender (pure Map.empty) $
    fromMaybe Map.empty <$> liftJSM (LS.retrieve LS.KeyStats)

  let evLSStats = evLSMapStats <&> (fmap ((False,) . (Nothing,))
         . Map.findWithDefault [] (navSystemLang, navCurrent))

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
      (  "class" =: "float-right text-zinc-500 hover:text-grayishblue-800 cursor-pointer"
      <> "title" =: "toggle sound"
      ) $ dyn_ $ dynSound <&> \case
        True  -> iFa "fas fa-volume-down"
        False -> iFa "fas fa-volume-mute"

    updateState $ domEvent Click domSound $> [field @"stApp" . field @"stSound" %~ not]
