{-# LANGUAGE GADTs #-}

module LocalStorage
  ( Key (..)
  , retrieve
  , put
  , update
  )
  where

import           Common.Model                   ( AppState
                                                , Stats, TextLang
                                                )
import           Control.Category               ( (<<<) )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.Aeson                    as Aeson
import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )
import qualified Data.Text.Lazy                as Lazy
import qualified Data.Text.Lazy.Encoding       as Lazy
import           GHCJS.DOM                      ( currentWindowUnchecked )
import           GHCJS.DOM.Storage              ( getItem
                                                , setItem
                                                )
import           GHCJS.DOM.Window               ( getLocalStorage )
import           Language.Javascript.JSaddle    ( MonadJSM )
import           Palantype.Common               ( StageIndex
                                                , SystemLang
                                                )
import           State                          ( Session )
import           TextShow                       ( TextShow(..)
                                                , fromText
                                                )
import Text.Pandoc.Definition (Pandoc)
import Data.Time (UTCTime)

data Key a where
  KeyAppState :: Key AppState
  KeySession  :: Key Session
  KeyStats    :: Key (Map (SystemLang, StageIndex) [Stats])
  KeyCMSCache :: Key (Map (SystemLang, TextLang, Text) (UTCTime, [Pandoc]))

instance TextShow (Key a) where
  showb = fromText. \case
    KeyAppState -> "app-state"
    KeySession  -> "session"
    KeyStats    -> "stats"
    KeyCMSCache -> "cms-cache"

retrieve :: (MonadJSM m, FromJSON a) => Key a -> m (Maybe a)
retrieve key =
  currentWindowUnchecked >>= getLocalStorage >>= \s ->
    (decode =<<) <$> getItem s (showt key)

put :: (MonadJSM m, ToJSON a) => Key a -> a -> m ()
put key d =
  currentWindowUnchecked >>= getLocalStorage >>= \s ->
    setItem s (showt key) $ encode d

update :: (MonadJSM m, FromJSON a, ToJSON a) => Key a -> (Maybe a -> a) -> m ()
update key f = do
  mOld <- retrieve key
  put key $ f mOld

decode :: forall a. FromJSON a => Text -> Maybe a
decode = Aeson.decode <<< Lazy.encodeUtf8 <<< Lazy.fromStrict

encode :: forall a. ToJSON a => a -> Text
encode = Lazy.toStrict <<< Lazy.decodeUtf8 <<< Aeson.encode
