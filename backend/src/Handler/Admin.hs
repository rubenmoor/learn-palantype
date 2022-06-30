{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Admin where

import           Servant.Server                 ( err500
                                                , errBody
                                                , throwError
                                                , HasServer(ServerT)
                                                )
import           Common.Api                     ( RoutesAdmin )
import           AppData                        ( Handler )
import           Auth                           ( UserInfo(..) )
import           Common.Model                   ( Journal(..) )
import           Database.Gerippe               (desc, orderBy, (&&.), (<=.), from, select, val, (>=.), (==.), (^.), where_, InnerJoin (..),  keyToId
                                                , joinMTo1Where'
                                                , Entity(..)
                                                )
import qualified DbAdapter                     as Db
import           Database                       ( blobDecode
                                                , runDb
                                                )
import           Data.Maybe                     ( Maybe(..), fromMaybe)
import           Control.Monad                  (Monad((>>=)) )
import           Data.Traversable               (for )
import           Control.Applicative            ( Applicative(pure) )
import           Data.Function                  ( ($) )
import Database.Gerippe (on)
import Data.Time (addDays, Day, UTCTime (..), getCurrentTime)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (Bool)
import Data.Int (Int)
import Data.Text (Text)

handlers :: ServerT RoutesAdmin a Handler
handlers =
    (handleJournalGetAll)

handleJournalGetAll :: UserInfo -> Maybe Day -> Maybe Day -> Bool -> Maybe Int -> Maybe Text -> Maybe Text -> Handler [Journal]
handleJournalGetAll UserInfo {..} mStart mEnd bExcludeAdmin mVisitorId mUser mAlias = do
    now <- liftIO getCurrentTime
    let end = fromMaybe (utctDay now) mEnd
        start = fromMaybe (addDays (-7) end) mStart
    es <- runDb $ select $ from $ \(j `InnerJoin` v) -> do
            on $ j ^. Db.JournalFkVisitor ==. v ^. Db.VisitorId
            where_ $ j ^. Db.JournalCreated >=. val (UTCTime start 0)
                 &&. j ^. Db.JournalCreated <=. val (UTCTime (addDays 1 end) 0)
            -- when excludeAdmin $ where_ $ not_ $ j ^. Db.JournalFkMAlias ==. val (Just uiKeyAlias)
            orderBy [desc $ j ^. Db.JournalCreated]
            pure (j, v)
    for es \(Entity _ Db.Journal {..}, Entity _ Db.Visitor {..}) -> do
            let journalVisitorId = keyToId journalFkVisitor
                journalTime = journalCreated
            journalEvent      <- blobDecode journalBlob
            journalMAliasUser <- case journalFkMAlias of
                Nothing -> pure Nothing
                Just keyAlias -> runDb (joinMTo1Where' Db.AliasFkUser
                                                       Db.UserId
                                                       Db.AliasId
                                                       keyAlias
                                       ) >>= \case
                    [(Entity _ Db.Alias {..}, Entity _ Db.User {..})] ->
                        pure $ Just (aliasName, userName)
                    _ -> throwError $ err500
                        { errBody = "handleJournalGetAll: expected single entry"
                        }
            pure $ Journal { .. }
