{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module State where

-- import           Common.Auth      (SessionData)
import           Common.Api                     ( PloverCfg, defaultPloverCfg
                                                )
import           Common.Route                   ( FrontendRoute(..)
                                                )
import           Common.Stage                   (Stage ())
import Common.Auth (SessionData)
import           Control.Applicative            (pure, (<*>), (<$>) )
import           Data.Aeson                     (object, (.=), withObject, (.:),  FromJSON(..)
                                                , ToJSON(..)
                                                )
import           Data.Bool                      ( Bool(..) )
import           Data.Foldable                  ( Foldable(foldMap) )
import           Data.Function                  ( ($) )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( Maybe(..) )
import           Data.Semigroup                 (Endo(..) )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Obelisk.Route                  ( pattern (:/)
                                                , R
                                                )
import           Palantype.Common               (Lang (..),  Chord )
import           Reflex.Dom                     ( EventWriter(..)
                                                , Reflex(Dynamic, Event)
                                                )
import Text.Read (read)
import Data.Time (UTCTime, NominalDiffTime)
import Data.Int (Int)
import TextShow (TextShow(showt))
import Text.Show (Show(show))
import Data.String (String)

-- environment for frontend pages

data Env t key = Env
    { envDynState   :: Dynamic t State

  -- ideally, envEChord would have the type `Event t (Chord k)`
  -- however, that requires a choice of k (e.g. k ~ DE.Key)
    , envEChord     :: Event t (Chord key)
    , envNavigation :: Navigation
    }

data Navigation = Navigation
    { navLang      :: Lang
    , navMPrevious :: Maybe Stage
    , navCurrent   :: Stage
    , navMNext     :: Maybe Stage
    }

-- frontend application state

-- newtype EStateUpdate
--   = EStateUpdate { unEStateUpdate :: State -> State }
--
-- instance Semigroup EStateUpdate where
--   u <> v =
--     let u' = unEStateUpdate u
--         v' = unEStateUpdate v
--     in  EStateUpdate $ v' . u'

-- State

data Stats = Stats
    { statsDate :: UTCTime
    , statsTime :: NominalDiffTime
    , statsLength :: Int
    , statsNErrors :: Int
    }
    deriving Generic

instance FromJSON Stats
instance ToJSON Stats


data State = State
    { stSession :: Session
    , stApp     :: AppState
    , stRedirectUrl :: R FrontendRoute
    }
    deriving Generic

instance FromJSON State where
  parseJSON = withObject "State" \s -> do
    stSession <- s .: "stSession"
    stApp     <- s .: "stApp"
    let stRedirectUrl = FrontendRoute_Main :/ ()
    pure State{..}

instance ToJSON State where
  toJSON State{..} = object
    [ "stSession" .= stSession
    , "stApp"     .= stApp
    ]

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
    deriving Generic

instance FromJSON AppState
instance ToJSON AppState

defaultProgress :: Map Lang Stage
defaultProgress =
    let stage_introduction = read "introduction"
    in  Map.fromList [(EN, stage_introduction), (DE, stage_introduction)]

defaultState :: State
defaultState = State
    { stSession       = SessionAnon
    , stApp = defaultAppState
    , stRedirectUrl = FrontendRoute_Main :/ ()
    }

defaultAppState :: AppState
defaultAppState = AppState
    { stCleared       = Set.empty
    , stMLang         = Nothing
    , stMsg           = Nothing
    , stPloverCfg     = defaultPloverCfg
    , stProgress      = defaultProgress
    , stStats         = Map.empty
    , stShowKeyboard  = True
    , stKeyboardShowQwerty = True
    , stShowTOC       = False
    , stTOCShowStage1 = False
    , stTOCShowStage2 = False
    , stTOCShowStage3 = False
    , stTOCShowStage4 = False
    }

updateState
    :: (Reflex t, EventWriter t (Endo State) m, Foldable l)
    => Event t (l (State -> State))
    -> m ()
updateState event = tellEvent $ foldMap Endo <$> event

data Message = Message
    { msgCaption :: Text
    , msgBody    :: Text
    }
    deriving Generic

instance FromJSON Message
instance ToJSON Message

-- Session

data Session
  = SessionUser SessionData
  | SessionAnon
  deriving (Generic)

instance FromJSON Session
instance ToJSON Session

stageUrl :: Lang -> Stage -> R FrontendRoute
stageUrl lang stage =
    case lang of
        EN -> FrontendRoute_EN :/ stage
        DE -> FrontendRoute_DE :/ stage
