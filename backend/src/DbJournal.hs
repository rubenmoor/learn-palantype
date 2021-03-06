{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module DbJournal where

import Data.Maybe (fromMaybe, Maybe (..))
import Control.Applicative ((<$>), Applicative(pure))
import qualified Data.Text.Encoding as Text
import Database.Gerippe (Key, insert_, getBy, Entity(Entity))
import qualified Database.Gerippe
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Time (getCurrentTime)
import Control.Category ((.))
import Snap.Core (getHeader, getRequest)
import Data.Function (($))
import Control.Monad (Monad((>>=)))

import qualified DbAdapter as Db
import Common.Model (JournalEvent)
import AppData (Handler)
import Database ( runDb, blobEncode )

insert :: Maybe (Key Db.Alias) -> JournalEvent -> Handler ()
insert mKeyAlias journal = do
  now <- liftIO getCurrentTime
  ip <- Text.decodeUtf8 . fromMaybe "unknown" . getHeader "X-Forwarded-For" <$> getRequest
  keyVisitor <- runDb (getBy $ Db.UIpAddress ip) >>= \case
    Just (Entity k _) -> pure k
    Nothing           ->
      runDb $ Database.Gerippe.insert $ Db.Visitor ip
  runDb $ insert_ $ Db.Journal now (blobEncode journal) keyVisitor mKeyAlias
