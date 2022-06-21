{-# LANGUAGE DeriveGeneric         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.Model
  ( module Common.Model
  ) where

import           Data.Aeson              (FromJSON (..), ToJSON (..))
import           Data.Text               (Text)
import           Data.Time               (UTCTime)
import           GHC.Generics            (Generic)

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
