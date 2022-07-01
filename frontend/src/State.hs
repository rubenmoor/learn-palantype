{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module State where

import           Common.Route                   ( FrontendRoute(..) )
import           Common.Stage                   ( Stage() )
import           Common.Auth                    ( SessionData )
import           Control.Applicative            ( (<$>)
                                                )
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )
import           Data.Foldable                  ( Foldable(foldMap) )
import           Data.Function                  ( ($) )
import           Data.Maybe                     ( Maybe(..) )
import           Data.Semigroup                 ( Endo(..) )
import           GHC.Generics                   ( Generic )
import Data.Map.Strict (Map)
import           Obelisk.Route                  ( pattern (:/)
                                                , R
                                                )
import           Palantype.Common               ( Lang(..)
                                                , Chord
                                                )
import           Reflex.Dom                     ( EventWriter(..)
                                                , Reflex(Dynamic, Event)
                                                )
import Common.Model (Stats, defaultAppState, AppState)
import qualified Data.Map.Strict as Map

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


data State = State
    { stSession     :: Session
    , stApp         :: AppState
    , stStats       :: Map (Lang, Stage) [Stats]
    , stRedirectUrl :: R FrontendRoute
    }
    deriving Generic

defaultState :: State
defaultState = State { stSession     = SessionAnon
                     , stApp         = defaultAppState
                     , stStats       = Map.empty
                     , stRedirectUrl = FrontendRoute_Main :/ ()
                     }

updateState
    :: (Reflex t, EventWriter t (Endo State) m, Foldable l)
    => Event t (l (State -> State))
    -> m ()
updateState event = tellEvent $ foldMap Endo <$> event

-- Session

data Session
  = SessionUser SessionData
  | SessionAnon
  deriving (Generic)

instance FromJSON Session
instance ToJSON Session

stageUrl :: Lang -> Stage -> R FrontendRoute
stageUrl lang stage = case lang of
    EN -> FrontendRoute_EN :/ stage
    DE -> FrontendRoute_DE :/ stage
