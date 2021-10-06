{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}

module State where

-- import           Common.Auth      (SessionData)
import           Common.Alphabet     (PTChord)
import           Common.Api          (PloverCfg)
import           Common.Route        (FrontendRoute (..))
import           Control.Applicative (Applicative (pure, (<*>)), (<$>))
import           Control.Category    (Category ((.)))
import           Data.Aeson          (FromJSON (..), KeyValue ((.=)),
                                      ToJSON (..), Value (..), object, (.:))
import           Data.Aeson.Types    (prependFailure, typeMismatch)
import           Data.Bool           (Bool (..))
import           Data.Default        (Default (..))
import           Data.Eq             (Eq)
import           Data.Function       (($))
import           Data.Functor        (Functor (fmap))
import           Data.Maybe          (Maybe (..))
import           Data.Ord            (Ord)
import           Data.Semigroup      (Semigroup (..))
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           Obelisk.Route       (pattern (:/), R)
import           Reflex.Dom          (EventWriter (..), Reflex (Dynamic, Event))
import           Text.Show           (Show (..))

-- environment for frontend pages

data Env t = Env
  { envDynState :: Dynamic t State
  , envEChord   :: Event t PTChord
  , envNavigation :: Navigation
  }

data Navigation = Navigation
  { navMPrevious :: Maybe Stage
  , navCurrent :: Stage
  , navMNext :: Maybe Stage
  }

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
    , stProgress = def
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
  = Introduction
  | Stage1_1
  | Stage1_2
  | Stage1_3
  | Stage1_4
  | Stage1_5
  | Stage1_6
  | Stage1_7
  | Stage2_1
  deriving (Eq, Ord, Generic)

instance ToJSON Stage
instance FromJSON Stage

instance Default Stage where
  def = Introduction

instance Show Stage where
  show Introduction = "Introduction"
  show Stage1_1     = "Stage 1.1"
  show Stage1_2     = "Stage 1.2"
  show Stage1_3     = "Stage 1.3"
  show Stage1_4     = "Stage 1.4"
  show Stage1_5     = "Stage 1.5"
  show Stage1_6     = "Stage 1.6"
  show Stage1_7     = "Stage 1.7"
  show Stage2_1     = "Stage 2.1"

stageUrl :: Stage -> R FrontendRoute
stageUrl = \case
  Introduction -> FrontendRoute_Introduction :/ ()
  Stage1_1 -> FrontendRoute_Stage1_1 :/ ()
  Stage1_2 -> FrontendRoute_Stage1_2 :/ ()
  Stage1_3 -> FrontendRoute_Stage1_3 :/ ()
  Stage1_4 -> FrontendRoute_Stage1_4 :/ ()
  Stage1_5 -> FrontendRoute_Stage1_5 :/ ()
  Stage1_6 -> FrontendRoute_Stage1_6 :/ ()
  Stage1_7 -> FrontendRoute_Stage1_7 :/ ()
  Stage2_1 -> FrontendRoute_Stage2_1 :/ ()
