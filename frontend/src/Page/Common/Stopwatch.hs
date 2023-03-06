{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Page.Common.Stopwatch
    ( mkStopwatch
    , elStopwatch
    , elStatistics
    , elStatisticsPersonalShort
    , StateStopwatch(..)
    , toMinutes
    , ElStatsFlag (..)
    )
where

import           Client                         ( getAuthData
                                                , postStatsStart
                                                , request
                                                )
import           Common.Model                   ( AppState(..)
                                                , ShowStats(..)
                                                , Stats(..)
                                                )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( (.)
                                                , (<<<)
                                                )
import           Control.Lens.Setter            ( (.~) )
import           Control.Monad                  ( Monad((>>=))
                                                , unless
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Reader           ( MonadReader
                                                , ask
                                                )
import           Data.Bool                      ( Bool )
import           Data.Eq                        ( Eq((==)) )
import           Data.Foldable                  ( Foldable(minimum, null)
                                                , for_
                                                )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( ($>)
                                                , (<$>)
                                                , (<&>)
                                                , Functor(fmap)
                                                , void
                                                )
import           Data.Generics.Product          ( field )
import           Data.Int                       ( Int )
import           Data.List                      ( filter
                                                , take
                                                )
import           Data.Maybe                     ( Maybe(..)
                                                , isNothing
                                                )
import           Data.Semigroup                 ( Endo
                                                , Semigroup((<>))
                                                )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           Data.Time                      ( NominalDiffTime
                                                , UTCTime
                                                , defaultTimeLocale
                                                , diffUTCTime
                                                , getCurrentTime
                                                )
import qualified Data.Time                     as Time
import           Data.Tuple                     ( fst
                                                , snd
                                                )
import           GHC.Float                      ( Double )
import           GHC.Real                       ( (/)
                                                , fromIntegral
                                                , realToFrac
                                                , round
                                                )
import           Reflex.Dom                     ( (=:)
                                                , DomBuilder
                                                , EventName(Click)
                                                , EventWriter
                                                , HasDomEvent(domEvent)
                                                , MonadHold
                                                , PerformEvent
                                                    ( Performable
                                                    , performEvent
                                                    )
                                                , PostBuild
                                                , Prerender
                                                , Reflex(Dynamic, Event)
                                                , TickInfo(_tickInfo_lastUTC)
                                                , TriggerEvent
                                                , blank
                                                , dyn
                                                , dyn_
                                                , el
                                                , elAttr
                                                , elClass
                                                , elClass'
                                                , foldDyn
                                                , holdUniqDyn
                                                , leftmost
                                                , text
                                                , tickLossyFromPostBuildTime
                                                , zipDyn
                                                )
import           Shared                         ( formatTime )
import           State                          ( Env(..)
                                                , State(..)
                                                , updateState
                                                )
import           TextShow                       ( showt )
import qualified Witherable
import           Witherable                     ( Filterable(catMaybes) )

data StateStopwatch
    = SWInitial
    | SWRun  UTCTime NominalDiffTime
    | SWStop UTCTime NominalDiffTime Int
    deriving stock Eq

data EventStopwatch
    = ESWToggle Int UTCTime
    | ESWTick UTCTime

elStopwatch
    :: forall key t (m :: * -> *)
     . ( PostBuild t m
       , DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadReader (Env t key) m
       )
    => Dynamic t [(Bool, (Maybe Text, Stats))]
    -> Dynamic t StateStopwatch
    -> Int
    -> m (Event t Stats)
elStopwatch dynStats dynStopwatch n = do
    evStats <-
        elClass "span" "stopwatch"
        $   fmap catMaybes
        $   dyn
        $   dynStopwatch <&> \case
                SWInitial              -> pure Nothing
                SWRun _ t              -> text (formatTime t) $> Nothing
                SWStop start t nErrors -> do
                    pure $ Just $ Stats { statsDate    = start
                                        , statsTime    = t
                                        , statsLength  = n
                                        , statsNErrors = nErrors
                                        }

    Env {..} <- ask

    el "hr" blank
    elClass "div" "stats small" $ do

        let elStrongWhen ss x = dyn_ $ envDynState <&> \st ->
              if stShowStats (stApp st) == ss
              then el "strong" x
              else x
        text "10 most recent scores ("
        (domPersonal, _) <- elClass' "a" "normalLink" $
          elStrongWhen ShowStatsPersonal $ text "personal"
        text " | "
        (domPublic, _) <- elClass' "a" "normalLink" $
          elStrongWhen ShowStatsPublic $ text "public"
        text " | "
        (domHide, _) <- elClass' "a" "normalLink" $
          elStrongWhen ShowStatsHide $ text "hide"
        text ")"
        let evPersonal = domEvent Click domPersonal $> ShowStatsPersonal
            evPublic   = domEvent Click domPublic   $> ShowStatsPublic
            evHide     = domEvent Click domHide     $> ShowStatsHide
        updateState $ leftmost [evPersonal, evPublic, evHide] <&> \ss ->
            [field @"stApp" . field @"stShowStats" .~ ss]

        dyn_ $ zipDyn (stShowStats . stApp <$> envDynState) dynStats <&> \case
            (ShowStatsHide    , _ ) -> blank
            (ShowStatsPersonal, ls) -> do
              let lsPersonal = filter (isNothing . fst . snd) ls
              unless (null lsPersonal) $ el "p" $ do
                text "Personal best: "
                elClass "span" "stopwatch" $ text $ formatTime
                  (minimum $ statsTime . snd . snd <$> ls)
              el "div" $ elStatistics ElStatsPersonal lsPersonal
            (ShowStatsPublic, ls) -> do
              let lsPersonal = filter (isNothing . fst . snd) ls
              unless (null lsPersonal) $ el "p" $ do
                text "Personal best: "
                elClass "span" "stopwatch" $ text $ formatTime
                  (minimum $ statsTime . snd . snd <$> ls)
              el "div" $ elStatistics ElStatsPublic ls


    pure evStats

mkStopwatch
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , MonadHold t m
       , MonadIO (Performable m)
       , MonadFix m
       , MonadReader (Env t key) m
       , PerformEvent t m
       , Prerender t m
       , PostBuild t m
       , TriggerEvent t m
       )
    => Event t Int
    -> m (Dynamic t StateStopwatch)
mkStopwatch ev = do
    Env{..} <- ask
    let evGo = void $ Witherable.filter (== (-1)) ev
    _ <- request $ postStatsStart (getAuthData <$> envDynState) evGo
    evToggle <- performEvent $ ev <&> \nErrors ->
        ESWToggle nErrors <$> liftIO getCurrentTime
    evTick <- fmap (ESWTick <<< _tickInfo_lastUTC)
        <$> tickLossyFromPostBuildTime 0.1

    let startStop :: EventStopwatch -> StateStopwatch -> StateStopwatch
        startStop (ESWTick timeTick) (SWRun timeStart _) =
            SWRun timeStart $ diffUTCTime timeTick timeStart
        startStop (ESWTick _) sw = sw
        startStop (ESWToggle nErrors timeStop) (SWRun timeStart _) =
            SWStop timeStart (diffUTCTime timeStop timeStart) nErrors
        startStop (ESWToggle _ timeStart) _ = SWRun timeStart 0

    foldDyn startStop SWInitial (leftmost [evToggle, evTick]) >>= holdUniqDyn

toMinutes :: NominalDiffTime -> Double
toMinutes t = realToFrac t / 60

data ElStatsFlag
  = ElStatsPersonal
  | ElStatsPublic

-- TODO prevent table cells from word wrap
elStatistics
    :: forall t (m :: * -> *)
     . (DomBuilder t m)
    => ElStatsFlag
    -> [(Bool, (Maybe Text, Stats))]
    -> m ()
elStatistics flag ls = do
  let lsShow = snd <$> case flag of
        ElStatsPublic   -> filter fst ls
        ElStatsPersonal -> ls
  elClass "table" "statistics" $
    for_ lsShow \(mAlias, Stats {..}) -> el "tr" $ do
          elClass "td" "date" $ text $ Text.pack $ Time.formatTime
              defaultTimeLocale
              "%F %R"
              statsDate
          elClass "td" "time" $ text $ formatTime statsTime
          case flag of
              ElStatsPersonal -> blank
              ElStatsPublic   -> elClass "td" "alias" $ case mAlias of
                  Just alias -> el "strong" $ text alias
                  Nothing    -> el "em" $ text "you"
          elClass "td" "nMistakes" $ do
              if statsNErrors == 0
                  then elAttr "strong" ("title" =: "0 mistakes")
                      $ text "flawless"
                  else text $ showt statsNErrors <> " mistakes"
          elClass "td" "wpm"
              $  text
              $  showt @Int
                     (round $ fromIntegral statsLength / toMinutes statsTime)
              <> " wpm"

elStatisticsPersonalShort
    :: forall t (m :: * -> *)
     . (DomBuilder t m)
    => [Stats]
    -> m ()
elStatisticsPersonalShort ls =
  elClass "table" "statistics" $
    for_ (take 3 ls) \Stats {..} -> el "tr" $ do
            elClass "td" "date" $ text $ Text.pack $ Time.formatTime
                defaultTimeLocale
                "%F %R"
                statsDate
            elClass "td" "time" $ text $ formatTime statsTime
            elClass "td" "nMistakes" $
                if statsNErrors == 0
                    then elAttr "strong" ("title" =: "0 mistakes")
                        $ text "flawless"
                    else text $ showt statsNErrors <> " mistakes"
            elClass "td" "wpm"
                $  text
                $  showt @Int
                       (round $ fromIntegral statsLength / toMinutes statsTime)
                <> " wpm"
