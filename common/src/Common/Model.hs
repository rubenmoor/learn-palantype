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
import           Common.Stage                   ( Stage )
import Data.Int (Int64)
import TextShow (fromText, TextShow (showb))

-- frontend/Localstorage

data AppState = AppState
    { stCleared       :: Set Stage
    , stMLang         :: Maybe Lang
    , stMsg           :: Maybe Message
    , stPloverCfg     :: PloverCfg
    , stShowKeyboard  :: Bool
    , stKeyboardShowQwerty :: Bool
    , stShowTOC       :: Bool
    , stProgress      :: Map Lang Stage
    , stStats         :: Map (Lang, Stage) [Stats]
    , stTOCShowStage1 :: Bool
    , stTOCShowStage2 :: Bool
    , stTOCShowStage3 :: Bool
    , stTOCShowStage4 :: Bool
    }
    deriving (Eq, Generic, Show)

instance FromJSON AppState
instance ToJSON AppState

defaultAppState :: AppState
defaultAppState = AppState { stCleared            = Set.empty
                           , stMLang              = Nothing
                           , stMsg                = Nothing
                           , stPloverCfg          = defaultPloverCfg
                           , stProgress           = defaultProgress
                           , stStats              = Map.empty
                           , stShowKeyboard       = True
                           , stKeyboardShowQwerty = True
                           , stShowTOC            = False
                           , stTOCShowStage1      = False
                           , stTOCShowStage2      = False
                           , stTOCShowStage3      = False
                           , stTOCShowStage4      = False
                           }

data Message = Message
    { msgCaption :: Text
    , msgBody    :: Text
    }
    deriving (Eq, Generic, Show)

instance FromJSON Message
instance ToJSON Message

defaultProgress :: Map Lang Stage
defaultProgress =
    let stage_introduction = read "introduction"
    in  Map.fromList [(EN, stage_introduction), (DE, stage_introduction)]

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

data Anon = Anon
  { anonId :: Int64
  }
  deriving (Generic, Eq, Ord)

instance ToJSON Anon
instance FromJSON Anon

instance TextShow Anon where
  showb Anon{..} = fromText "anon" <> showb anonId

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
  | EventStageCompleted Stage Stats
  deriving (Generic)

instance ToJSON EventApp
instance FromJSON EventApp
