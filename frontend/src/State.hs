{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module State where

-- import           Common.Auth      (SessionData)
import           Common.Api          (PloverCfg)
import           Control.Applicative (Applicative (pure, (<*>)), (<$>))
import           Control.Category    (Category ((.)))
import           Data.Aeson          (FromJSON (..), KeyValue ((.=)),
                                      ToJSON (..), Value (..), object, (.:))
import           Data.Aeson.Types    (prependFailure, typeMismatch)
import           Data.Bool           (Bool(..))
import           Data.Default        (Default (..))
import           Data.Function       (($))
import           Data.Functor        (Functor (fmap))
import           Data.Maybe          (Maybe (..))
import           Data.Semigroup      (Semigroup (..))
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           Reflex.Dom          (EventWriter (..), Reflex (Event))
import Common.Route (FrontendRoute(..))
import Obelisk.Route (R, pattern (:/))

-- frontend application state

newtype EStateUpdate
  = EStateUpdate { unEStateUpdate :: State -> State }

instance Semigroup EStateUpdate where
  u <> v =
    let u' = unEStateUpdate u
        v' = unEStateUpdate v
    in  EStateUpdate $ v' . u'

-- State

data State = State
  { -- stSession :: Session
    stPloverCfg    :: PloverCfg
  , stMsg          :: Maybe Message
  , stShowKeyboard :: Bool
  , stProgress     :: Stage
  } deriving (Generic)

instance FromJSON State where
  parseJSON (Object o) =
    State <$> o .: "ploverCfg"
          <*> pure Nothing -- don't expect a persisted message
          <*> o .: "keyboard"
          <*> o .: "progress"
  parseJSON invalid =
    prependFailure "parsing State failed, " $ typeMismatch "Object" invalid

instance ToJSON State where
  toJSON State{..} =
    object
    [ "ploverCfg" .= stPloverCfg
    -- stMsg: never persist messages
    , "keyboard" .= stShowKeyboard
    , "progress" .= stProgress
    ]

instance Default State where
  def = State
    { stPloverCfg = def
    , stMsg = Nothing
    , stShowKeyboard = True
    , stProgress = Stage1_1
    }

updateState ::
  ( Reflex t
  , EventWriter t EStateUpdate m
  ) => Event t (State -> State) -> m ()
updateState event =
  tellEvent $ fmap EStateUpdate event

data Message = Message
  { msgCaption :: Text
  , msgBody    :: Text
  }
-- -- Session
--
-- data Session
--   = SessionUser SessionData
--   | SessionAnon
--   deriving (Generic)
--
-- instance FromJSON Session
-- instance ToJSON Session

data Stage
  = Stage1_1
  | Stage1_2
  | Stage2_1
  deriving (Generic)

instance ToJSON Stage
instance FromJSON Stage

stageUrl :: Stage -> R FrontendRoute
stageUrl = \case
  Stage1_1 -> FrontendRoute_Stage1_1 :/ ()
  Stage1_2 -> FrontendRoute_Stage1_2 :/ ()
  Stage2_1 -> FrontendRoute_Stage2_1 :/ ()
