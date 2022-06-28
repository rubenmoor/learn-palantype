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
import           Database.Gerippe               ( keyToId
                                                , joinMTo1Where'
                                                , joinMTo1'
                                                , Entity(..)
                                                )
import qualified DbAdapter                     as Db
import           Database                       ( blobDecode
                                                , runDb
                                                )
import           Data.Maybe                     ( Maybe(..) )
import           Control.Monad                  ( Monad((>>=)) )
import           Data.Traversable               ( Traversable(traverse) )
import           Control.Applicative            ( Applicative(pure) )
import           Data.Function                  ( ($) )

handlers :: ServerT RoutesAdmin a Handler
handlers =
    (handleJournalGetAll
    -- :<|> handleStageCompleted
                        )

handleJournalGetAll :: UserInfo -> Handler [Journal]
handleJournalGetAll UserInfo {..} =
    runDb (joinMTo1' Db.JournalFkVisitor Db.VisitorId) >>= traverse
        \(Entity _ Db.Journal {..}, Entity _ Db.Visitor {..}) -> do
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
