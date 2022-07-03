{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
module Handler.Event where

import Servant.Server (HasServer(ServerT))
import Servant.API (toUrlPiece, (:<|>)(..))
import Common.Api (RoutesEvent)
import AppData (Handler)
import Auth (UserInfo (..))
import Data.Text (Text)
import qualified DbJournal
import Common.Model (EventApp (..), Stats (..), JournalEvent (..))
import Common.Stage (Stage)
import Palantype.Common (Lang)
import qualified DbAdapter as Db
import qualified Data.Text as Text
import Database.Gerippe (insert_)
import Database (runDb)

handlers :: ServerT RoutesEvent a Handler
handlers =
  (
         handleViewPage
    :<|> handleStageCompleted
  )

handleViewPage :: Maybe UserInfo -> Text -> Handler ()
handleViewPage mUi strPath =
  DbJournal.insert (uiKeyAlias <$> mUi) $ EventApp $ EventViewPage strPath

handleStageCompleted :: Maybe UserInfo -> (Lang, Stage, Stats) -> Handler ()
handleStageCompleted mUi (lang, stage, stats@Stats{..}) = do
  DbJournal.insert (uiKeyAlias <$> mUi) $ EventApp $
    EventStageCompleted lang stage stats
  whenJust mUi $ \UserInfo{..} ->
    runDb $ insert_ $ Db.Stats uiKeyAlias
                               statsDate
                               (realToFrac statsTime)
                               (toUrlPiece lang)
                               (Text.pack $ show stage)
                               statsLength
                               statsNErrors

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust m f = case m of
  Just x -> f x
  Nothing -> pure ()
