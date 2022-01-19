{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module State where

-- import           Common.Auth      (SessionData)
import           Common.Api                     ( PloverCfg
                                                )
import           Common.Route                   ( FrontendRoute(..)
                                                , Stage (..)
                                                )
import           Control.Applicative            ( (<$>) )
import           Control.Category               ( (<<<) )
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                )
import           Data.Bool                      ( Bool(..) )
import           Data.Default                   ( Default(..) )
import           Data.Eq                        ( Eq )
import           Data.Foldable                  ( Foldable(foldMap) )
import           Data.Function                  ( ($) )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( Maybe(..) )
import           Data.Ord                       ( Ord )
import           Data.Semigroup                 ((<>),  Endo(..) )
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
import           TextShow                       ( TextShow(showb)
                                                , fromText
                                                )

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
    { -- stSession :: Session
      stCleared       :: Set Stage
    , stMLang         :: Maybe Lang
    , stMsg           :: Maybe Message
    , stPloverCfg     :: PloverCfg
    , stShowKeyboard  :: Bool
    , stShowTOC       :: Bool
    , stProgress      :: Map Lang Stage
    , stTOCShowStage1 :: Bool
    , stTOCShowStage2 :: Bool
    , stTOCShowStage3 :: Bool
    }
    deriving Generic

instance FromJSON State
instance ToJSON State

instance Default State where
    def = State
        { stCleared       = Set.empty
        , stMLang         = Nothing
        , stMsg           = Nothing
        , stPloverCfg     = def
        , stProgress = Map.fromList [(EN, Stage "introduction"), (DE, Stage "introduction")]
        , stShowKeyboard  = True
        , stShowTOC       = False
        , stTOCShowStage1 = False
        , stTOCShowStage2 = False
        , stTOCShowStage3 = False
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

-- -- Session
--
-- data Session
--   = SessionUser SessionData
--   | SessionAnon
--   deriving (Generic)
--
-- instance FromJSON Session
-- instance ToJSON Session

stageUrl :: Lang -> Stage -> R FrontendRoute
stageUrl lang stage =
    case lang of
        EN -> FrontendRoute_EN :/ stage
        DE -> FrontendRoute_DE :/ stage

stageDescription :: Stage -> Text
stageDescription (Stage stage) = case stage of
    "introduction" -> "Introduction"
    "stage_1-1"     -> "Ex. 1: Type the letters"
    "stage_1-2"     -> "Ex. 2: Memorize the order"
    "stage_1-3"     -> "Ex. 3: Type the letters blindly"
    "stage_1-4"     -> "Ex. 4: Memorize the order blindly"
    "stage_1-5"     -> "Ex. 5: Memorize the left hand"
    "stage_1-6"     -> "Ex. 6: Memorize the right hand"
    "stage_1-7"     -> "Ex. 7: Memorize them all"
    "stage_2-1"     -> "Ex. 1: Make use of home row"
    "stage_2-2"     -> "Ex. 2: Learn your first chords"
    "stage_2-3"     -> "Ex. 3: Onset, nucleus, and coda"
    "stage_2-4"     -> "Ex. 4: Syllables and word parts"
    "patternoverview" -> "Pattern overview"
    other           -> "Page not found: " <> other
