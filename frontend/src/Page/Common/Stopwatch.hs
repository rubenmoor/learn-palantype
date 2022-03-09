{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Page.Common.Stopwatch
    ( mkStopwatch
    , elStopwatch
    , elStatistics
    , formatTime
    , StateStopwatch (..)
    , toMinutes
    ) where

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Function (($))
import Data.Functor ((<&>), ($>), (<$>))
import Data.Int (Int)
import Data.Semigroup
    ( Semigroup ((<>)),
    )
import Data.Text (Text)
import GHC.Float (Double)
import GHC.Real (div, mod, realToFrac, (/), fromIntegral, round, floor)
import Reflex.Dom
import TextShow (showt)
import Data.Functor (Functor(fmap))
import Data.Time (defaultTimeLocale, diffUTCTime, UTCTime, NominalDiffTime, getCurrentTime)
import qualified Data.Text as Text
import Text.Printf (printf)
import Control.Category ((<<<), (.))
import Data.Witherable (Filterable(catMaybes))
import State (stStats, Env (..), Stats (..), Navigation (..))
import qualified Data.Map.Strict as Map
import Control.Monad.Reader (ask, MonadReader)
import Control.Monad (when)
import Data.Eq (Eq)
import Data.Maybe (Maybe(..))
import Control.Applicative (Applicative(pure))
import Data.Foldable (for_, Foldable(minimum))
import Data.Bool (Bool(..))
import qualified Data.Time as Time
import Data.Ord (Ord((>)))
import GHC.Num ((-), Num((*)))
import Control.Monad (Monad((>>=)))

data StateStopwatch
    = SWInitial
    | SWRun  UTCTime NominalDiffTime
    | SWStop UTCTime NominalDiffTime
    deriving stock Eq

data EventStopwatch
    = ESWToggle UTCTime
    | ESWTick UTCTime

elStopwatch
  :: forall key t (m :: * -> *)
  . ( PostBuild t m
    , DomBuilder t m
    , MonadFix m
    , MonadHold t m
    , MonadReader (Env t key) m
    )
  => Dynamic t StateStopwatch
  -> Int
  -> m (Event t Stats)
elStopwatch dynStopwatch n = do
    evStats <- elClass "span" "stopwatch" $ fmap catMaybes $ dyn $ dynStopwatch <&> \case
        SWInitial      -> pure Nothing
        SWRun  _     t -> text (formatTime t) $> Nothing
        SWStop start t -> do
            pure $ Just $ Stats
                { statsDate = start
                , statsTime = t
                , statsLength = n
                }

    Env {..} <- ask
    let Navigation {..} = envNavigation
    let dynMStats =  Map.lookup (navLang, navCurrent) . stStats <$> envDynState
    dyn_ $ dynMStats <&> \case
        Nothing -> blank
        Just ls -> elClass "div" "paragraph stopwatch" $ do
            el "hr" blank
            text $ "Best: "
            elClass "span" "stopwatch" $ text $ formatTime (minimum $ statsTime <$> ls)
            dynStats <- elClass "span" "small" $ do
                text "("
                (elToggle, _) <- elClass' "a" "normalLink" $ text "personal stats"
                text ")"
                toggle False (domEvent Click elToggle)
            dyn_ $ dynStats <&> \show -> when show $ el "div" $ elStatistics ls


    pure evStats

mkStopwatch
  :: forall t (m :: * -> *) a
  . ( DomBuilder t m
    , MonadHold t m
    , MonadIO (Performable m)
    , MonadFix m
    , PerformEvent t m
    , PostBuild t m
    , TriggerEvent t m
    )
  => Event t a
  -> m (Dynamic t StateStopwatch)
mkStopwatch ev = do
    evToggle <- performEvent $ ev $> (ESWToggle <$> liftIO getCurrentTime)
    evTick <- fmap (ESWTick <<< _tickInfo_lastUTC) <$> tickLossyFromPostBuildTime 0.1

    let startStop :: EventStopwatch -> StateStopwatch -> StateStopwatch
        startStop (ESWTick timeTick  ) (SWRun timeStart _) =
            SWRun timeStart $ diffUTCTime timeTick timeStart
        startStop (ESWTick _) sw = sw
        startStop (ESWToggle timeStop) (SWRun timeStart _) =
            SWStop timeStart $ diffUTCTime timeStop timeStart
        startStop (ESWToggle timeStart) _ = SWRun timeStart 0

    foldDyn startStop SWInitial (leftmost [evToggle, evTick]) >>= holdUniqDyn

-- in time 1.8.0.2 there is not FormatTime instance for Difftime
-- (or NominalDifftime)
-- and GHC 8.6.5 depends on that one specifically
formatTime :: NominalDiffTime -> Text
formatTime dt =
    let seconds = realToFrac dt
        secondsFull = floor @Double @Int seconds
        secondsTenth = floor @Double @Int $ (seconds - fromIntegral secondsFull) * 10
        minutes = secondsFull `div` 60
        strMinutes = if minutes > 0 then printf "%2d:" minutes else ""
        strSeconds = printf "%02d." $ secondsFull `mod` 60
    in  Text.pack (strMinutes <> strSeconds) <> showt secondsTenth <> "s"

toMinutes :: NominalDiffTime -> Double
toMinutes t = realToFrac t / 60

elStatistics
    :: forall t (m :: * -> *)
     . ( DomBuilder t m )
    => [Stats]
    -> m ()
elStatistics ls = elClass "table" "statistics" $ do
    el "caption" $ text "Statistics"
    for_ ls \Stats{..} -> el "tr" $ do
        elClass "td" "date" $ text $ Text.pack $ Time.formatTime defaultTimeLocale "%F %R" statsDate
        elClass "td" "time" $ text $ formatTime statsTime
        elClass "td" "wpm"  $ text $ showt @Int (round $ fromIntegral statsLength / toMinutes statsTime) <> " wpm"
