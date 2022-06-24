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
  | RankOwner
  deriving (Generic, Eq, Ord)

instance FromJSON Rank
instance ToJSON Rank

-- Journal

data Journal = Journal
  { journalCreated     :: UTCTime
  , journalSubject     :: Subject
  , journalEvent       :: Event
  , journalDescription :: Text
  } deriving (Generic)

instance ToJSON Journal
instance FromJSON Journal

data Event
  = EventView
  | EventLogin
  | EventLogout
  | EventCreation
  | EventEdit
  deriving (Generic)

instance ToJSON Event
instance FromJSON Event

-- | subject of journal entry
data Subject
  = SubjectUser -- cases: new user, user changes password, receives new clearance
  | SubjectAlias
  | SubjectEpisode
  | SubjectPodcast
  deriving (Generic)

instance ToJSON Subject
instance FromJSON Subject
