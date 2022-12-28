{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Stats
    ( handlers
    )
where

import           Palantype.Common               ( Lang )
import           Common.Stage                   ( StageIndex )
import           Common.Model                   ( JournalEvent (EventApp),  Stats(..), EventApp (..) )
import           AppData                        ( Handler )
import           Servant.Server                 (err500, throwError,  ServantErr (errBody),  HasServer(ServerT) )
import           Common.Api                     ( RoutesStats )
import           Auth                           ( UserInfo(..) )
import           Database.Gerippe               (deleteBy, getBy, insert_,  Entity(..)
                                                , on
                                                , just
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
import qualified Palantype.DE as DE
import qualified Palantype.EN as EN
import Palantype.Common (Palantype, Lang (..))
import Palantype.Common.TH (fromJust)
import qualified Common.Stage as Stage
import Common.Stage (Stage, StageIndex, toStageRepr)


handlers :: ServerT RoutesStats a Handler
handlers =
         handleStatsGet
    :<|> handleStatsStart
    :<|> handleStatsCompleted

handleStatsGet
    :: Maybe UserInfo -> Lang -> StageIndex -> Handler [(Maybe Text, Stats)]
handleStatsGet mUi lang iStage = do
    let mStageRepr = case lang of
          DE -> toStageRepr @DE.Key <$> Stage.fromIndex iStage
          EN -> toStageRepr @EN.Key <$> Stage.fromIndex iStage
    es <- runDb $ select $ from $ \(s `InnerJoin` a) -> do
        on $ s ^. Db.StatsFkAlias ==. a ^. Db.AliasId
        where_ $ a ^. Db.AliasIsVisible
             &&. s ^. Db.StatsLang ==. val (toUrlPiece lang)
             &&. just (s ^. Db.StatsStageRepr) ==. val mStageRepr
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

handleStatsCompleted :: Maybe UserInfo -> (Lang, StageIndex, Stats) -> Handler ()
handleStatsCompleted mUi (lang, iStage, stats@Stats{..}) = do
  let stageRepr = case lang of
        DE -> toStageRepr @DE.Key $ $fromJust $ Stage.fromIndex iStage
        EN -> toStageRepr @EN.Key $ $fromJust $ Stage.fromIndex iStage
  DbJournal.insert (uiKeyAlias <$> mUi) $ EventApp $
    EventStageCompleted lang stageRepr stats
  whenJust mUi $ \UserInfo{..} -> do
    now <- liftIO getCurrentTime
    runDb (getBy $ Db.UFkAlias uiKeyAlias) >>= \case
      Nothing -> throwError $ err500 { errBody = "could not store stats (1)" }
      Just (Entity _ (Db.StageBegin _ created)) -> do
          if abs (diffUTCTime now created - statsTime) < 5
          then runDb $ insert_ $ Db.Stats uiKeyAlias
                                          statsDate
                                          (realToFrac statsTime)
                                          (toUrlPiece lang)
                                          stageRepr
                                          statsLength
                                          statsNErrors
          else throwError $ err500 { errBody = "could not store stats (2)" }
          runDb $ deleteBy $ Db.UFkAlias uiKeyAlias

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust m f = case m of
  Just x -> f x
  Nothing -> pure ()
