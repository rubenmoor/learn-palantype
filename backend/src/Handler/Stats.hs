{-# LANGUAGE RecordWildCards #-}
module Handler.Stats
  ( handlers
  ) where

import Palantype.Common (Lang)
import Common.Stage (Stage)
import Common.Model (Stats (..) )
import AppData ( Handler )
import Servant.Server (HasServer(ServerT))
import Common.Api (RoutesStats)
import Auth (UserInfo (..))
import Database.Gerippe (Entity (..), on, where_, val, (==.), (^.), (&&.), from, select, InnerJoin(InnerJoin))
import Database (runDb)
import qualified DbAdapter as Db
import qualified Data.Text as Text
import Servant.API (ToHttpApiData(toUrlPiece))
import Data.Functor ((<&>))
import Data.Text (Text)

handlers :: ServerT RoutesStats a Handler
handlers = handleStatsGet

handleStatsGet :: Maybe UserInfo -> Lang -> Stage -> Handler [(Maybe Text, Stats)]
handleStatsGet mUi lang stage = do
  es <- runDb $ select $ from $ \(s `InnerJoin` a) -> do
    on $ s ^. Db.StatsFkAlias ==. a ^. Db.AliasId
    where_ $ a ^. Db.AliasIsVisible
         &&. s ^. Db.StatsLang ==. val (toUrlPiece lang)
         &&. s ^. Db.StatsStage ==. val (Text.pack $ show stage)
    pure (s, a)
  pure $ es <&> \(Entity _ Db.Stats{..}, Entity _ Db.Alias{..}) ->
    ( case mUi of
        Just UserInfo{..} ->
          if Db.aliasName uiAlias == aliasName then Nothing else Just aliasName
        Nothing           -> Just aliasName
    , Stats statsCreated
          (realToFrac statsTime)
          statsLength
          statsNErrors
    )

-- handleGetCurrentTimeSigned :: UserInfo -> Handler CompactJWS
-- handleGetCurrentTimeSigned UserInfo{..} = do
--   now <- liftIO getCurrentTime
--   EnvApplication{..} <- ask
--   signJWS now [(newJWSHeader (Protected, alg), envJwk)]
