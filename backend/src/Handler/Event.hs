{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Handler.Event where

import Servant.Server (HasServer(ServerT))
import Common.Api (RoutesEvent)
import AppData (Handler)
import Auth (UserInfo (..))
import Data.Text (Text)
import qualified DbJournal
import Common.Model (EventApp (..), JournalEvent (..))

handlers :: ServerT RoutesEvent a Handler
handlers =
  (
         handleViewPage
  )

handleViewPage :: Maybe UserInfo -> Text -> Handler ()
handleViewPage mUi strPath =
  DbJournal.insert (uiKeyAlias <$> mUi) $ EventApp $ EventViewPage strPath
