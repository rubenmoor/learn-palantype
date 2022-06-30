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
import           Database.Gerippe               ((||.), (!=.), just, (?.), valkey, not_, desc, orderBy, (&&.), (<=.), from, select, val, (>=.), (==.), (^.), where_, InnerJoin (..),  keyToId, LeftOuterJoin (..)
                                                , joinMTo1Where'
                                                , Entity(..)
                                                )
import qualified DbAdapter                     as Db
import           Database                       ( blobDecode
                                                , runDb
                                                )
import           Data.Maybe                     ( Maybe(..), fromMaybe)
import           Control.Monad                  (when, Monad((>>=)) )
import           Data.Traversable               (for )
import           Control.Applicative            ( Applicative(pure) )
import           Data.Function                  ( ($) )
import Database.Gerippe (on)
import Data.Time (addDays, Day, UTCTime (..), getCurrentTime)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (Bool)
import Data.Int (Int)
import Data.Text (Text)
import GHC.Real (fromIntegral)
import Database.Gerippe (isNothing)

handlers :: ServerT RoutesAdmin a Handler
handlers =
    (handleJournalGetAll)

handleJournalGetAll :: UserInfo -> Maybe Day -> Maybe Day -> Bool -> Maybe Int -> Maybe Text -> Maybe Text -> Handler [Journal]
handleJournalGetAll UserInfo {..} mStart mEnd bExcludeAdmin mVisitorId mUser mAlias = do
    now <- liftIO getCurrentTime
    let end = fromMaybe (utctDay now) mEnd
        start = fromMaybe (addDays (-7) end) mStart
    es <- runDb $ select $ from $ \(j `InnerJoin` v `LeftOuterJoin` ma) -> do
            on $ j ^. Db.JournalFkMAlias ==. ma ?. Db.AliasId
            on $ j ^. Db.JournalFkVisitor ==. v ^. Db.VisitorId
            where_ $ j ^. Db.JournalCreated >=. val (UTCTime start 0)
                 &&. j ^. Db.JournalCreated <=. val (UTCTime (addDays 1 end) 0)
            when bExcludeAdmin $
              where_ $ isNothing (j ^. Db.JournalFkMAlias)
                   ||. j ^. Db.JournalFkMAlias !=. val (Just uiKeyAlias)
            whenJust mVisitorId $ \visitorId ->
              where_ $ v ^. Db.VisitorId ==. valkey (fromIntegral visitorId)
            whenJust mAlias $ \alias ->
              where_ $ ma ?. Db.AliasName ==. just (val alias)
            whenJust mUser $ \userName -> pure ()
            orderBy [desc $ j ^. Db.JournalCreated]
            pure (j, v, ma)
    for es \(Entity _ Db.Journal {..}, Entity _ Db.Visitor {..}, ma) -> do
            let journalVisitorId = keyToId journalFkVisitor
                journalTime = journalCreated
            journalEvent      <- blobDecode journalBlob
            journalMAliasUser <- case ma of
                Nothing -> pure Nothing
                Just (Entity keyAlias alias) ->
                  runDb (joinMTo1Where' Db.AliasFkUser Db.UserId Db.AliasId keyAlias) >>= \case
                    [(_, Entity _ Db.User {..})] ->
                        pure $ Just (Db.aliasName alias, userName)
                    _ -> throwError $ err500
                        { errBody = "handleJournalGetAll: expected single entry"
                        }
            pure $ Journal { .. }

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust x f = case x of
  Just x  -> f x
  Nothing -> pure ()
