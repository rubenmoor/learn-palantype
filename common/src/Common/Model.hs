{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.Model
    ( module Common.Model
    )
where

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )
import           Data.Text                      ( Text )
import           Data.Time                      ( NominalDiffTime
                                                , UTCTime
                                                )
import           GHC.Generics                   ( Generic )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Palantype.Common               (Lang (..))

import Common.PloverConfig ( PloverCfg, defaultPloverCfg )
import           Palantype.Common               (StageRepr,  StageIndex )

-- frontend/Localstorage

data AppState = AppState
    { stCleared       :: Set StageIndex
    , stMLang         :: Maybe Lang
    , stMsg           :: Maybe Message
    , stPloverCfg     :: PloverCfg
    , stShowKeyboard  :: Bool
    , stKeyboardShowQwerty :: Bool
    , stShowTOC       :: Bool
    , stProgress      :: Map Lang StageIndex
    , stTOCShowStage  :: Set Int
    , stShowStats     :: ShowStats
    , stSound         :: Bool
    }
    deriving (Eq, Generic, Show)

instance FromJSON AppState
instance ToJSON AppState

defaultAppState :: AppState
defaultAppState = AppState { stCleared            = Set.empty
                           , stMLang              = Nothing
                           , stMsg                = Nothing
                           , stPloverCfg          = defaultPloverCfg
                           , stShowKeyboard       = True
                           , stKeyboardShowQwerty = True
                           , stShowTOC            = False
                           , stProgress           = defaultProgress
                           , stTOCShowStage       = Set.empty
                           , stShowStats          = ShowStatsHide
                           , stSound              = False
                           }

data Message = Message
    { msgCaption :: Text
    , msgBody    :: Text
    }
    deriving (Eq, Generic, Show)

instance FromJSON Message
instance ToJSON Message

data ShowStats
  = ShowStatsHide
  | ShowStatsPersonal
  | ShowStatsPublic
  deriving (Eq, Generic, Show)

instance FromJSON ShowStats
instance ToJSON ShowStats


defaultProgress :: Map Lang StageIndex
defaultProgress = Map.fromList [(EN, 0), (DE, 0)]

-- frontend and backend

data Stats = Stats
    { statsDate :: UTCTime
    , statsTime :: NominalDiffTime
    , statsLength :: Int
    , statsNErrors :: Int
    }
    deriving (Eq, Generic, Show)

instance FromJSON Stats
instance ToJSON Stats

-- backend

data Rank
  = RankMember
  | RankModerator
  | RankAdmin
  deriving (Generic, Eq, Ord)

instance FromJSON Rank
instance ToJSON Rank

-- Journal

data Journal = Journal
  { journalEvent :: JournalEvent
  , journalVisitorId :: Int
  , journalVisitorIp :: Text
  , journalMAliasUser :: Maybe (Text, Text)
  , journalTime :: UTCTime
  } deriving (Generic)

instance FromJSON Journal
instance ToJSON Journal

data JournalEvent
  = EventUser EventUser
  | EventApp  EventApp
  deriving (Generic)

instance ToJSON JournalEvent
instance FromJSON JournalEvent

data EventUser
  = EventLogin
  | EventLogout
  | EventSignup
  | EventEdit Text Text Text
  | EventDelete
  deriving (Generic)

instance ToJSON EventUser
instance FromJSON EventUser

data EventApp
  = EventViewPage Text
  | EventStageCompleted Lang StageRepr Stats
  deriving (Generic)

instance ToJSON EventApp
instance FromJSON EventApp
