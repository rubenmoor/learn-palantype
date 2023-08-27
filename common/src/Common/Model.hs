{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Model
    ( module Common.Model
    ) where

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import           Data.Time                      ( NominalDiffTime
                                                , UTCTime
                                                )
import           GHC.Generics                   ( Generic )
import           Palantype.Common               ( StageIndex
                                                , StageRepr
                                                , SystemLang(..)
                                                )

import           Common.PloverConfig            ( PloverCfg
                                                , defaultPloverCfg
                                                )
import           Web.HttpApiData                ( FromHttpApiData
                                                    ( parseQueryParam, parseUrlPiece
                                                    )
                                                , ToHttpApiData(toQueryParam, toUrlPiece)
                                                )
import qualified Data.Text as Text
import Text.Read (Read(readPrec))
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import Text.ParserCombinators.ReadP (string)
import Data.Foldable (asum)
import Data.Functor (($>))
import TextShow

newtype UTCTimeInUrl = UTCTimeInUrl { unUTCTimeInUrl :: UTCTime }

instance ToHttpApiData UTCTimeInUrl where
  toUrlPiece = Text.replace ":" "_" . toUrlPiece . unUTCTimeInUrl

instance FromHttpApiData UTCTimeInUrl where
  parseUrlPiece = fmap UTCTimeInUrl . parseUrlPiece . Text.replace "_" ":"

-- frontend/Localstorage

data AppState = AppState
    { stMLang     :: Maybe SystemLang
    , stMsg       :: Maybe Message
    , stKeyboard  :: StateKeyboard
    , stPloverCfg :: PloverCfg
    , stToc       :: StateToc
    , stShowStats :: ShowStats
    , stSound     :: Bool
    }
    deriving (Eq, Generic, Show)

instance FromJSON AppState
instance ToJSON AppState

defaultAppState :: AppState
defaultAppState = AppState
  { stMLang     = Nothing
  , stMsg       = Nothing
  , stKeyboard  = defaultStateKeyboard
  , stPloverCfg  = defaultPloverCfg
  , stToc       = defaultStateToc
  , stShowStats = ShowStatsHide
  , stSound     = False
  }

data StateKeyboard = StateKeyboard
    { stShow       :: Bool
    , stShowQwerty :: Bool
    , stModes      :: Bool
    , stActive     :: Bool
    , stTocNavigation :: TocNavigation
    }
    deriving (Eq, Generic, Show)

instance FromJSON StateKeyboard
instance ToJSON   StateKeyboard

defaultStateKeyboard :: StateKeyboard
defaultStateKeyboard = StateKeyboard
    { stShow       = True
    , stShowQwerty = True
    , stModes      = False
    , stActive     = True
    , stTocNavigation = TocNavToplevel
    }

data TocNavigation = TocNavToplevel | TocNavSublevel Int
    deriving (Eq, Generic, Show)

instance FromJSON TocNavigation
instance ToJSON   TocNavigation

data StateToc = StateToc
    { stVisible       :: Bool
    , stCleared       :: Set StageIndex
    , stShowToplevelSteno :: Bool
    , stShowSublevelSteno :: Bool
    , stProgress      :: Map SystemLang StageIndex
    , stShowStage     :: Set Int
    }
    deriving (Eq, Generic, Show)

instance FromJSON StateToc
instance ToJSON   StateToc

defaultStateToc :: StateToc
defaultStateToc = StateToc
  { stVisible           = False
  , stCleared           = Set.empty
  , stShowToplevelSteno = False
  , stShowSublevelSteno = False
  , stProgress          = defaultProgress
  , stShowStage         = Set.empty
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


defaultProgress :: Map SystemLang StageIndex
defaultProgress = Map.fromList [(SystemEN, 0), (SystemDE, 0)]

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
  | EventStageCompleted SystemLang StageRepr Stats
  deriving (Generic)

instance ToJSON EventApp
instance FromJSON EventApp

-- cms

data TextLang
  = TextDE
  | TextEN
  deriving (Eq, Generic, Ord)

instance ToJSON TextLang
instance FromJSON TextLang

instance TextShow TextLang where
  showb = \case
    TextDE -> fromText "TextDE"
    TextEN -> fromText "TextEN"

instance Read TextLang where
  readPrec = ReadPrec.lift $ asum $ (\(str, lang) -> string str $> lang) <$>
    [ ("TextDE", TextDE)
    , ("TextEN", TextEN)
    ]

instance ToHttpApiData TextLang where
  toQueryParam = \case
    TextDE -> "TextDE"
    TextEN -> "TextEN"

instance FromHttpApiData TextLang where
  parseQueryParam = \case
    "TextDE" -> Right TextDE
    "TextEN" -> Right TextEN
    str     -> Left $ "FromHttpApiData: Couldn't parse TextLang: " <> str

data CacheContentType = CacheContentMarkdown
  deriving (Eq, Generic, Ord)

instance ToJSON CacheContentType
instance FromJSON CacheContentType
