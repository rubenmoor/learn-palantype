{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
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
import           Palantype.Common               ( StageIndex )
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
import           Obelisk.Route                  ( pattern (:/)
                                                , R
                                                )
import           Palantype.Common               (Palantype,  Lang(..)
                                                , Chord
                                                )
import           Reflex.Dom                     ( EventWriter(..)
                                                , Reflex(Dynamic, Event)
                                                )
import Common.Model (defaultAppState, AppState)
import Type.Reflection (eqTypeRep, (:~~:)(HRefl), typeRep)
import qualified Palantype.DE as DE
import qualified Palantype.EN as EN
import Palantype.Common.TH (failure)
import Data.Bool (otherwise)

-- environment for frontend pages

data Env t key = Env
    { envDynState   :: Dynamic t State

  -- ideally, envEChord would have the type `Event t (Chord k)`
  -- however, that requires a choice of k (e.g. k ~ DE.Key)
    , envEChord     :: Event t (Chord key)
    , envNavigation :: Navigation
    , envToReady    :: Event t () -> Event t ()
    }

data Navigation = Navigation
    { navLang      :: Lang
    , navMPrevious :: Maybe StageIndex
    , navCurrent   :: StageIndex
    , navMNext     :: Maybe StageIndex
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
    , stRedirectUrl :: R FrontendRoute
    }
    deriving Generic

defaultState :: State
defaultState = State { stSession     = SessionAnon
                     , stApp         = defaultAppState
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

stageUrl :: forall key. Palantype key => StageIndex -> R FrontendRoute
stageUrl iStage
    | Just HRefl <- typeRep @key `eqTypeRep` typeRep @DE.Key = FrontendRoute_DE :/ iStage
    | Just HRefl <- typeRep @key `eqTypeRep` typeRep @EN.Key = FrontendRoute_EN :/ iStage
    | otherwise = $failure "key not implemented"
