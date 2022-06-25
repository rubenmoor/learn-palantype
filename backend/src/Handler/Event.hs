{-# LANGUAGE DataKinds #-}
module Handler.Event where

import Servant.Server (HasServer(ServerT))
import Servant.API ((:<|>)(..))
import Snap.Core (Snap)
import Common.Api (RoutesEvent)
import AppData (Handler)
import Auth (UserInfo (..))
import Data.Text (Text)
import qualified DbJournal
import Common.Model (Stats, EventApp (..), Journal(JournalApp))
import Common.Stage (Stage)

handlers :: ServerT RoutesEvent a Handler
handlers =
  (
         handleViewPage
    :<|> handleStageCompleted
  )

handleViewPage :: Maybe UserInfo -> Text -> Handler ()
handleViewPage mUi strPath =
  DbJournal.insert (uiKeyAlias <$> mUi) $ JournalApp $ EventViewPage strPath

handleStageCompleted :: Maybe UserInfo -> (Stage, Stats) -> Handler ()
handleStageCompleted mUi (stage, stats) =
  DbJournal.insert (uiKeyAlias <$> mUi) $ JournalApp $
    EventStageCompleted stage stats
