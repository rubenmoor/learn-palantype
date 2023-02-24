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
import Database.Gerippe
    ( (||.),
      (!=.),
      just,
      (?.),
      valkey,
      delete,
      desc,
      orderBy,
      (&&.),
      (<=.),
      from,
      select,
      val,
      (>=.),
      (==.),
      (^.),
      where_,
      InnerJoin(..),
      keyToId,
      LeftOuterJoin(..),
      Entity(..),
      on,
      isNothing )
import qualified DbAdapter                     as Db
import           Database                       ( blobDecode
                                                , runDb
                                                )
import           Data.Maybe                     ( Maybe(..)
                                                , fromMaybe
                                                , catMaybes
                                                )
import           Control.Monad                  ( when
                                                )
import           Data.Traversable               ( for )
import           Control.Applicative            ( Applicative(pure) )
import           Data.Function                  ( ($) )
import           Data.Time                      ( addDays
                                                , Day
                                                , UTCTime(..)
                                                , getCurrentTime
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Bool                      ( Bool )
import           Data.Int                       ( Int )
import           Data.Text                      ( Text )
import           GHC.Real                       ( fromIntegral )
import Data.Functor ((<$>), ($>))

handlers :: ServerT RoutesAdmin a Handler
handlers = handleJournalGetAll

handleJournalGetAll
    :: UserInfo
    -> Maybe Day
    -> Maybe Day
    -> Bool
    -> Maybe Int
    -> Maybe Text
    -> Maybe Text
    -> Bool
    -> Handler [Journal]
handleJournalGetAll UserInfo {..} mStart mEnd bExcludeAdmin mVisitorId mUser mAlias bAnonymous = do
    now <- liftIO getCurrentTime
    let end   = fromMaybe (utctDay now) mEnd
        start = fromMaybe (addDays (-7) end) mStart
    es <- runDb $ select $ from
        $ \(j `InnerJoin` v `LeftOuterJoin` ma `LeftOuterJoin` mu) -> do
              on $ ma ?. Db.AliasFkUser ==. mu ?. Db.UserId
              on $ j ^. Db.JournalFkMAlias ==. ma ?. Db.AliasId
              on $ j ^. Db.JournalFkVisitor ==. v ^. Db.VisitorId
              where_ $   j ^.  Db.JournalCreated >=. val (UTCTime start 0)
                     &&. j ^.  Db.JournalCreated <=. val (UTCTime (addDays 1 end) 0)
              when bExcludeAdmin $
                where_ $   isNothing (j ^. Db.JournalFkMAlias)
                ||. j ^.  Db.JournalFkMAlias !=. val (Just uiKeyAlias)
              whenJust mVisitorId $ \visitorId ->
                where_ $ v ^. Db.VisitorId ==. valkey (fromIntegral visitorId)
              whenJust mAlias $ \alias ->
                  where_ $ ma ?. Db.AliasName ==. just (val alias)
              whenJust mUser $ \userName ->
                  where_ $ mu ?. Db.UserName ==. just (val userName)
              when bAnonymous $
                where_ $ isNothing $ j ^. Db.JournalFkMAlias
              orderBy [desc $ j ^. Db.JournalCreated]
              pure (j, v, ma, mu)
    catMaybes <$> for es \(Entity key Db.Journal{..}, Entity _ Db.Visitor{..}, ma, mu) -> do
            let
                journalVisitorId = keyToId journalFkVisitor
                journalVisitorIp = visitorIpAddress
                journalTime      = journalCreated

            journalMAliasUser <- case (ma, mu) of
                (Nothing, Nothing) -> pure Nothing
                (Just (Entity _ Db.Alias {..}), Just (Entity _ Db.User {..}))
                    -> pure $ Just (aliasName, userName)
                _ -> throwError $ err500
                    { errBody = "handleJournalGetAll: expected single entry"
                    }
            case blobDecode journalBlob of
              Nothing -> runDb (delete key) $> Nothing -- instead of migration
                -- throwError $ err500
                -- { errBody = "Could not decode journal blob: "
                --     <> LazyText.encodeUtf8 (LazyText.fromStrict strErr)
                -- }
              Just journalEvent     -> pure $ Just $ Journal { .. }

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust m f = case m of
    Just x  -> f x
    Nothing -> pure ()
