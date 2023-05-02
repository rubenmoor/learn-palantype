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
{-# LANGUAGE RankNTypes #-}

module State where

import           Common.Auth                    ( SessionData )
import           Common.Model                   ( AppState
                                                , defaultAppState, TextLang
                                                )
import           Common.Route                   ( FrontendRoute(..), FrontendRoute_TextLang (..))
import           Control.Applicative            ( (<$>) )
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )
import           Data.Bool                      ( otherwise )
import           Data.Foldable                  ( Foldable(foldMap) )
import           Data.Function                  ( ($) )
import           Data.Maybe                     ( Maybe(..) )
import           Data.Semigroup                 ( Endo(..) )
import           GHC.Generics                   ( Generic )
import           Obelisk.Route                  ( pattern (:/)
                                                , R
                                                )
import           Palantype.Common               ( Chord
                                                , Palantype
                                                , StageIndex
                                                , SystemLang(..)
                                                )
import           Palantype.Common.TH            ( failure )
import qualified Palantype.DE                  as DE
import qualified Palantype.EN                  as EN
import           Reflex.Dom                     ( EventWriter(..)
                                                , Reflex(Dynamic, Event), MonadHold, PostBuild, PerformEvent (Performable), TriggerEvent
                                                )
import           Type.Reflection                ( (:~~:)(HRefl)
                                                , eqTypeRep
                                                , typeRep
                                                )
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class (MonadIO)
import Data.Eq (Eq)

type GetLoadedAndBuilt t =
  forall m
  . ( MonadHold t m
    , MonadIO (Performable m)
    , PerformEvent t m
    , PostBuild t m
    , TriggerEvent t m
    )
  => m (Event t ())

-- environment for frontend pages

data Env t key = Env
    { envDynState          :: Dynamic t State
      -- | Just c: a chord
      --   Nothing: the back-up chord
    , envEvMChord            :: Event t (Maybe (Chord key))
    , envNavigation        :: Navigation
    , envGetLoadedAndBuilt :: GetLoadedAndBuilt t
    }

data Navigation = Navigation
    { navSystemLang :: SystemLang
    , navMPrevious  :: Maybe StageIndex
    , navCurrent    :: StageIndex
    , navMNext      :: Maybe StageIndex
    , navPageName   :: Text
    , navTextLang   :: TextLang
    }

-- State

data Loading = LoadingDone | LoadingStill Text | LoadingError Text
  deriving (Eq, Generic)

data State = State
    { stSession     :: Session
    , stApp         :: AppState
    , stRedirectUrl :: R FrontendRoute
    , stCMSCacheInvalidationData :: Map (SystemLang, TextLang, Text) UTCTime
    , stLoading     :: Loading
    }
    deriving Generic

defaultState :: State
defaultState = State { stSession     = SessionAnon
                     , stApp         = defaultAppState
                     , stRedirectUrl = FrontendRoute_Main :/ ()
                     , stCMSCacheInvalidationData = Map.empty
                     , stLoading     = LoadingStill "Initialization"
                     }

updateState
    :: (Reflex t, EventWriter t (Endo State) m, Foldable l)
    => Event t (l (State -> State))
    -> m ()
updateState event = tellEvent $ foldMap Endo <$> event

-- Session

data Session where
  SessionUser :: SessionData -> Session
  SessionAnon :: Session
  deriving (Eq, Generic)

instance FromJSON Session
instance ToJSON Session

stageUrl :: forall key. Palantype key => StageIndex -> R FrontendRoute
stageUrl iStage
    | Just HRefl <- typeRep @key `eqTypeRep` typeRep @DE.Key = FrontendRoute_SystemDE :/ FrontendRoute_TextEN :/ iStage
    | Just HRefl <- typeRep @key `eqTypeRep` typeRep @EN.Key = FrontendRoute_SystemEN :/ FrontendRoute_TextEN :/ iStage
    | otherwise = $failure "key not implemented"
