{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module State where

-- import           Common.Auth      (SessionData)
import           Common.Api          (PloverCfg)
import           Control.Applicative (Applicative (pure, (<*>)), (<$>))
import           Control.Category    (Category ((.)))
import           Data.Aeson          (FromJSON (..), KeyValue ((.=)),
                                      ToJSON (..), Value (..), object, (.:))
import           Data.Aeson.Types    (prependFailure, typeMismatch)
import           Data.Default        (Default (..))
import           Data.Function       (($))
import           Data.Maybe          (Maybe(Just, Nothing))
import           Data.Semigroup      (Semigroup (..))
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import Reflex.Dom (EventWriter (..), Reflex(Event))
import Data.Functor (Functor(fmap))

-- frontend application state

newtype EStateUpdate
  = EStateUpdate { unEStateUpdate :: State -> State }

instance Semigroup EStateUpdate where
  u <> v =
    let u' = unEStateUpdate u
        v' = unEStateUpdate v
    in  EStateUpdate $ v' . u'

-- State

data State = State
  { -- stSession :: Session
    stPloverCfg :: PloverCfg
  , stMsg       :: Maybe Message
  , stKeyboard  :: Maybe ()
  } deriving (Generic)

instance FromJSON State where
  parseJSON (Object o) =
    State <$> o .: "ploverCfg"
          <*> pure Nothing -- don't expect a persisted message
          <*> o .: "keyboard"
  parseJSON invalid =
    prependFailure "parsing State failed, " $ typeMismatch "Object" invalid

instance ToJSON State where
  toJSON State{..} =
    object
    [ "ploverCfg" .= stPloverCfg
    -- stMsg: never persist messages
    , "keyboard" .= stKeyboard
    ]

instance Default State where
  def = State
    { stPloverCfg = def
    , stMsg = Nothing
    , stKeyboard = Just ()
    }

updateState ::
  ( Reflex t
  , EventWriter t EStateUpdate m
  ) => Event t (State -> State) -> m ()
updateState event =
  tellEvent $ fmap EStateUpdate event

data Message = Message
  { msgCaption :: Text
  , msgBody :: Text
  }
-- -- Session
--
-- data Session
--   = SessionUser SessionData
--   | SessionAnon
--   deriving (Generic)
--
-- instance FromJSON Session
-- instance ToJSON Session
