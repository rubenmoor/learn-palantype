{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module Page.Common.Stopwatch
    ( mkStopwatch
    , stoptimeMaybe
    , formatTime
    , StateStopwatch (..)
    ) where

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Function (($))
import Data.Functor (($>), (<$>))
import Data.Int (Int)
import Data.Maybe (Maybe (..))
import Data.Semigroup
    ( Semigroup ((<>)),
    )
import Data.Text (Text)
import GHC.Float (Double)
import Reflex.Dom
import TextShow (showt)
import Data.Functor (Functor(fmap))
import Data.Time (diffUTCTime, UTCTime, NominalDiffTime, getCurrentTime)
import qualified Data.Text as Text
import Text.Printf (printf)
import Control.Category ((<<<))

data StateStopwatch
    = SWInitial
    | SWRun UTCTime NominalDiffTime
    | SWStop  NominalDiffTime
    deriving stock Eq

data EventStopwatch
    = ESWToggle UTCTime
    | ESWTick UTCTime

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
            SWStop $ diffUTCTime timeStop timeStart
        startStop (ESWToggle timeStart) _ = SWRun timeStart 0

    foldDyn startStop SWInitial (leftmost [evToggle, evTick]) >>= holdUniqDyn

stoptimeMaybe :: StateStopwatch -> Maybe NominalDiffTime
stoptimeMaybe (SWStop t) = Just t
stoptimeMaybe _          = Nothing

-- in time 1.8.0.2 there is not FormatTime instance for Difftime
-- (or NominalDifftime)
-- and GHC 8.6.5 depends on that one specifically
formatTime :: NominalDiffTime -> Text
formatTime dt =
    let seconds = realToFrac dt
        secondsFull = floor @Double @Int seconds
        secondsTenth = floor @Double @Int $ (seconds - fromIntegral secondsFull) * 10
        minutes = secondsFull `div` 60
        strMinutes = if minutes > 0 then printf "%1d:" minutes else ""
        strSeconds = printf "%1d." $ secondsFull `mod` 60
    in  Text.pack (strMinutes <> strSeconds) <> showt secondsTenth <> "s"
