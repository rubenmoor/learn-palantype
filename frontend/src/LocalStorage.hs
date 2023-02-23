{-# LANGUAGE GADTs #-}

module LocalStorage
  ( Key (..)
  , retrieve
  , put
  )
  where

import           GHCJS.DOM                      ( currentWindowUnchecked )
import           GHCJS.DOM.Storage              ( getItem
                                                , setItem
                                                )
import           GHCJS.DOM.Window               (getLocalStorage)
import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy.Encoding as Lazy
import qualified Data.Text.Lazy as Lazy
import Language.Javascript.JSaddle (MonadJSM)
import Control.Category ((<<<))
import TextShow (TextShow (..), fromText)
import Common.Model (Stats, AppState)
import State (Session)
import Data.Map.Strict (Map)
import Palantype.Common (StageIndex)
import Palantype.Common (SystemLang)

data Key a where
  KeyAppState :: Key AppState
  KeySession  :: Key Session
  KeyStats    :: Key (Map (SystemLang, StageIndex) [Stats])

instance TextShow (Key a) where
  showb = fromText. \case
    KeyAppState -> "app-state"
    KeySession  -> "session"
    KeyStats    -> "stats"

retrieve :: (MonadJSM m, FromJSON a) => Key a -> m (Maybe a)
retrieve key =
  currentWindowUnchecked >>= getLocalStorage >>= \s ->
    fmap (decode =<<) $ getItem s (showt key)

put :: (MonadJSM m, ToJSON a) => Key a -> a -> m ()
put key d =
  currentWindowUnchecked >>= getLocalStorage >>= \s ->
    setItem s (showt key) $ encode d

decode :: forall a. FromJSON a => Text -> Maybe a
decode = Aeson.decode <<< Lazy.encodeUtf8 <<< Lazy.fromStrict

encode :: forall a. ToJSON a => a -> Text
encode = Lazy.toStrict <<< Lazy.decodeUtf8 <<< Aeson.encode
