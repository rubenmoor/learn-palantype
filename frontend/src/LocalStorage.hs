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
import Language.Javascript.JSaddle (ToJSVal (..), ToJSString (..), MonadJSM)
import Control.Category ((<<<))
import TextShow (TextShow (..), fromText)
import Common.Model (Stats, AppState)
import State (Session)
import Data.Map.Strict (Map)
import Common.Stage (Stage)
import Palantype.Common (Lang)

data Key a where
  KeyAppState :: Key AppState
  KeySession  :: Key Session
  KeyStats    :: Key (Map (Lang, Stage) [Stats])

instance TextShow (Key a) where
  showb = fromText. \case
    KeyAppState -> "app-state"
    KeySession  -> "session"
    KeyStats    -> "stats"

instance ToJSString (Key a) where
  toJSString = toJSString . showt

instance ToJSVal (Key a) where
  toJSVal = toJSVal . showt

retrieve :: (MonadJSM m, FromJSON a) => Key a -> m (Maybe a)
retrieve key =
  currentWindowUnchecked >>= getLocalStorage >>= \s ->
    fmap (decode =<<) $ getItem s key

put :: (MonadJSM m, ToJSON a) => Key a -> a -> m ()
put key d =
  currentWindowUnchecked >>= getLocalStorage >>= \s ->
    setItem s key $ encode d

decode :: forall a. FromJSON a => Text -> Maybe a
decode = Aeson.decode <<< Lazy.encodeUtf8 <<< Lazy.fromStrict

encode :: forall a. ToJSON a => a -> Text
encode = Lazy.toStrict <<< Lazy.decodeUtf8 <<< Aeson.encode
