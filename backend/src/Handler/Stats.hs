{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Stats
    ( handlers
    )
where

import           Palantype.Common               ( Lang )
import           Common.Stage                   ( Stage )
import           Common.Model                   ( JournalEvent (EventApp),  Stats(..), EventApp (..) )
import           AppData                        ( Handler )
import           Servant.Server                 ( HasServer(ServerT) )
import           Common.Api                     ( RoutesStats )
import           Auth                           ( UserInfo(..) )
import           Database.Gerippe               (deleteBy, delete, getBy, insert_,  Entity(..)
                                                , on
                                                , where_
                                                , val
                                                , (==.)
                                                , (^.)
                                                , (&&.)
                                                , from
                                                , select
                                                , InnerJoin(InnerJoin)
                                                )
import           Database                       ( runDb )
import qualified DbAdapter                     as Db
import qualified Data.Text                     as Text
import           Servant.API                    ( (:<|>)(..)
                                                , ToHttpApiData(toUrlPiece)
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Text                      ( Text )
import qualified DbJournal
import Data.Time (diffUTCTime, getCurrentTime)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad (when)

handlers :: ServerT RoutesStats a Handler
handlers =
         handleStatsGet
    :<|> handleStatsStart
    :<|> handleStatsCompleted

handleStatsGet
    :: Maybe UserInfo -> Lang -> Stage -> Handler [(Maybe Text, Stats)]
handleStatsGet mUi lang stage = do
    es <- runDb $ select $ from $ \(s `InnerJoin` a) -> do
        on $ s ^. Db.StatsFkAlias ==. a ^. Db.AliasId
        where_ $ a ^. Db.AliasIsVisible
             &&. s ^. Db.StatsLang      ==. val (toUrlPiece lang)
             &&. s ^. Db.StatsStage     ==. val (Text.pack $ show stage)
        pure (s, a)
    pure $ es <&> \(Entity _ Db.Stats {..}, Entity _ Db.Alias {..}) ->
        ( case mUi of
            Just UserInfo {..} -> if Db.aliasName uiAlias == aliasName
                then Nothing
                else Just aliasName
            Nothing -> Just aliasName
        , Stats statsCreated (realToFrac statsTime) statsLength statsNErrors
        )

handleStatsStart :: UserInfo -> Handler ()
handleStatsStart UserInfo {..} = do
  now <- liftIO getCurrentTime
  runDb $ do
    deleteBy $ Db.UFkAlias uiKeyAlias
    insert_ $ Db.StageBegin uiKeyAlias now

handleStatsCompleted :: Maybe UserInfo -> (Lang, Stage, Stats) -> Handler ()
handleStatsCompleted mUi (lang, stage, stats@Stats{..}) = do
  DbJournal.insert (uiKeyAlias <$> mUi) $ EventApp $
    EventStageCompleted lang stage stats
  whenJust mUi $ \UserInfo{..} -> do
    now <- liftIO getCurrentTime
    mStart <- runDb $ getBy $ Db.UFkAlias uiKeyAlias
    case mStart of
      Nothing -> runDb $ deleteBy $ Db.UFkAlias uiKeyAlias
      Just (Entity _ (Db.StageBegin _ created)) -> do
          when (abs (diffUTCTime now created - statsTime) < 5) $
              runDb $ insert_ $ Db.Stats uiKeyAlias
                                         statsDate
                                         (realToFrac statsTime)
                                         (toUrlPiece lang)
                                         (Text.pack $ show stage)
                                         statsLength
                                         statsNErrors
          runDb $ deleteBy $ Db.UFkAlias uiKeyAlias

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust m f = case m of
  Just x -> f x
  Nothing -> pure ()
