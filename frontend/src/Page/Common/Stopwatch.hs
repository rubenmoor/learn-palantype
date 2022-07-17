{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Page.Common.Stopwatch
    ( mkStopwatch
    , elStopwatch
    , elStatistics
    , StateStopwatch(..)
    , toMinutes
    , ElStatsFlag (..)
    )
where

import Client
import           Control.Lens.Setter            ( (.~) )
import           Data.Generics.Product          ( field )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<&>)
                                                , ($>)
                                                , (<$>)
                                                )
import           Data.Int                       ( Int )
import           Data.Semigroup                 ( Endo
                                                , Semigroup((<>))
                                                )
import           GHC.Float                      ( Double )
import           GHC.Real                       ( realToFrac
                                                , (/)
                                                , fromIntegral
                                                , round
                                                )
import           Reflex.Dom                     (Prerender,  EventWriter
                                                , zipDyn
                                                , DomBuilder
                                                , MonadHold
                                                , PostBuild
                                                , Reflex(Dynamic, Event)
                                                , leftmost
                                                , foldDyn
                                                , holdUniqDyn

                                                , tickLossyFromPostBuildTime
                                                , (=:)
                                                , blank
                                                , dyn
                                                , dyn_
                                                , el
                                                , elAttr
                                                , elClass
                                                , elClass'
                                                , text
                                                , PerformEvent
                                                    ( performEvent
                                                    , Performable
                                                    )
                                                , TickInfo(_tickInfo_lastUTC)
                                                , TriggerEvent
                                                , HasDomEvent(domEvent)
                                                , EventName(Click)
                                                )
import           TextShow                       ( showt )
import           Data.Functor                   ( Functor(fmap) )
import           Data.Time                      ( defaultTimeLocale
                                                , diffUTCTime
                                                , UTCTime
                                                , NominalDiffTime
                                                , getCurrentTime
                                                )
import qualified Data.Text                     as Text
import           Control.Category               ( (<<<)
                                                , (.)
                                                )
import           Data.Witherable                ( Filterable(catMaybes) )
import           State                          ( updateState
                                                , State(..)
                                                , Env(..)
                                                , Navigation(..)
                                                )
import           Control.Monad.Reader           ( ask
                                                , MonadReader
                                                )
import           Control.Monad                  ( when )
import           Data.Eq                        ( Eq
                                                , (==)
                                                )
import           Data.Maybe                     (isNothing,  Maybe(..) )
import           Control.Applicative            ( Applicative(pure) )
import           Data.Foldable                  ( for_
                                                , Foldable(minimum)
                                                )
import           Data.Bool                      (Bool, not )
import qualified Data.Time                     as Time
import           Control.Monad                  ( Monad((>>=)) )
import           Common.Model                   ( ShowStats(..)
                                                , Stats(..)
                                                , AppState(..)
                                                )
import           Shared                         ( formatTime )
import           Data.Text                      ( Text )
import           Data.Tuple                     (fst,  snd )
import Data.Foldable (Foldable(null))
import Data.List (filter)
import qualified Data.Witherable as Witherable
import Data.Functor (void)

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
    let Navigation {..} = envNavigation

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
              when (not $ null lsPersonal) $ el "p" $ do
                text $ "Personal best: "
                elClass "span" "stopwatch" $ text $ formatTime
                  (minimum $ statsTime . snd . snd <$> ls)
              el "div" $ elStatistics ElStatsPersonal lsPersonal
            (ShowStatsPublic, ls) -> do
              let lsPersonal = filter (isNothing . fst . snd) ls
              when (not $ null lsPersonal) $ el "p" $ do
                text $ "Personal best: "
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
        (ESWToggle nErrors <$> liftIO getCurrentTime)
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
  elClass "table" "statistics" $ do
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
