{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Handler.Admin where

import           AppData                        ( Handler, EnvApplication (..) )
import           Auth                           ( UserInfo(..) )
import           Common.Api                     ( RoutesAdmin )
import           Common.Model                   ( Journal(..) )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Monad                  ( when, unless, foldM )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Bool                      ( Bool )
import           Data.Foldable                  ( for_ )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( ($>)
                                                , (<$>)
                                                )
import           Data.Int                       ( Int )
import           Data.Maybe                     ( Maybe(..)
                                                , catMaybes
                                                , fromMaybe
                                                )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.IO                   ( putStrLn
                                                , writeFile
                                                )
import           Data.Time                      ( Day
                                                , UTCTime(..)
                                                , addDays
                                                , getCurrentTime
                                                )
import           Data.Traversable               ( for )
import           Database                       ( blobDecode
                                                , runDb
                                                )
import           Database.Gerippe               ( (!=.)
                                                , (&&.)
                                                , (<=.)
                                                , (==.)
                                                , (>=.)
                                                , (?.)
                                                , Entity(..)
                                                , InnerJoin(..)
                                                , LeftOuterJoin(..)
                                                , (^.)
                                                , delete
                                                , desc
                                                , from
                                                , isNothing
                                                , just
                                                , keyToId
                                                , on
                                                , orderBy
                                                , select
                                                , val
                                                , valkey
                                                , where_
                                                , (||.)
                                                )
import qualified DbAdapter                     as Db
import           GHC.Real                       ( fromIntegral )
import           Palantype.Common               ( Palantype(toDescription)
                                                , Stage(..)
                                                , StageHierarchy(..)
                                                , StageSpecialGeneric(..)
                                                , getSystemLang
                                                )
import           Palantype.Common.Stage         ( stages
                                                , toPageName
                                                )
import qualified Palantype.DE                  as DE
import qualified Palantype.EN                  as EN
import           Servant.API                    ( ToHttpApiData(toUrlPiece),type  (:<|>) ((:<|>)) )
import           Servant.Server                 ( HasServer(ServerT)
                                                , err500
                                                , errBody
                                                , throwError, err403
                                                )
import           System.Directory               ( doesFileExist )
import           TextShow                       ( TextShow(showt) )
import Obelisk.Configs (HasConfigs(getConfigs), runConfigsT, getTextConfig)
import Control.Monad.Reader (MonadReader(ask))
import Data.Eq (Eq((==)))
import GHC.Enum (Enum(succ))

handlers :: ServerT RoutesAdmin a Handler
handlers = handleJournalGetAll :<|> handleCreateMissingFilesLocally

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
    unless uiIsSiteAdmin $ throwError err403
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

-- | traverse the stages and check if a corresponding markdown file exists
--   if not, create it
handleCreateMissingFilesLocally :: UserInfo -> Handler ()
handleCreateMissingFilesLocally UserInfo{..} = do
    EnvApplication{..} <- ask
    unless (envUrl == "http://localhost:8000") $ throwError err500
      { errBody = "only meant for use in local development"
      }
    unless uiIsSiteAdmin $ throwError err403
    createMissingFiles @DE.Key
    createMissingFiles @EN.Key
  where
    createMissingFiles :: forall key. Palantype key => Handler ()
    createMissingFiles = liftIO do
        nTotal <- foldM accFunc (0 :: Int) stages
        putStrLn $ showt nTotal <> " files created."
      where
        accFunc counter stage@Stage{..} = do
          let
              systemLang = getSystemLang @key
              filename = "cms-content/"
                <> Text.unpack (toUrlPiece systemLang)
                <> "/EN/"
                <> Text.unpack (toPageName @key stage) <> ".md"
          bExists <- doesFileExist filename
          if bExists
            then do
              putStrLn $ "Skipping existing file: " <> Text.pack filename
              pure counter
            else do
              putStrLn $ "Creating file: " <> Text.pack filename
              case stageHierarchy of
                StageToplevel     -> case stageSpecialGeneric of
                  StageSpecial str -> writeFile filename $ "# " <> str
                  StageGeneric _ _ -> writeFile filename ""
                StageSublevel t s -> case stageSpecialGeneric of
                  StageSpecial str -> writeFile filename $ "# " <> str
                  StageGeneric pg g -> writeFile filename $
                      "# Stage " <> showt t <> "\n\n"
                    <> "### Exercise " <> showt s <> "\n\n"
                    <> "## " <> toDescription pg <> "\n\n"
                    <> "### G" <> showt g <> "\n"
              pure $ succ counter
