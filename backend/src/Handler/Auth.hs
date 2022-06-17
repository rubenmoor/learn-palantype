{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE LiberalTypeSynonyms  #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Handler.Auth
  ( handlers
  ) where

import           Control.Applicative       (Applicative (pure))
import           Control.Category          (Category ((.)))
import           Control.Exception.Lifted  (catch, evaluate)
import           Control.Monad             (Monad ((>>=)), unless, when)
import           Control.Monad.Except      (runExceptT)
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.Reader      (asks)
import           Control.Monad.Trans       (MonadTrans (lift))
import           Crypto.JWT                (JWK)
import           Data.Aeson                (decodeStrict, encode)
import           Data.Bool                 (Bool (..))
import qualified Data.ByteString.Lazy      as Lazy
import qualified Data.ByteString.Lazy.UTF8 as BSU
import           Data.Char                 (isAlphaNum)
import           Data.Either               (either)
import           Data.Eq                   (Eq ((==)))
import           Data.Foldable             (Foldable (foldl'))
import           Data.Function             (flip, ($))
import           Data.Functor              (Functor (fmap), (<$>))
import           Data.Int                  (Int)
import           Data.List                 (null, sortOn, (++))
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (Maybe (Just, Nothing), isJust,
                                            maybe)
import           Data.Monoid               ((<>))
import           Data.Ord                  (Down (Down), Ord ((<), (>)))
import           Data.Password             (mkPassword)
import           Data.Password.Argon2      (PasswordCheck (..), checkPassword,
                                            hashPassword)
import           Data.Pool                 (Pool)
import           Data.Text                 (Text, breakOn, drop, replace,
                                            toUpper)
import qualified Data.Text                 as Text
import           Data.Time                 (defaultTimeLocale, formatTime,
                                            getCurrentTime, parseTimeM)
import           Data.Traversable          (traverse)
import           Data.Tuple                (snd)
import           Database.Gerippe          (Entity (..), InnerJoin (..), Key,
                                            PersistStoreWrite (insert, insert_),
                                            PersistUniqueRead (getBy),
                                            PersistentSqlException, from, get,
                                            getAll, getWhere, joinMTo1Where',
                                            on, select, val, where_, (&&.),
                                            (==.), (^.))
import           Database.Persist.MySQL    (PersistStoreWrite (update),
                                            SqlBackend, runSqlPool, (=.))

import           Safe                      (headMay)
import           Servant.API               ((:<|>) (..),
                                            FromHttpApiData (parseHeader))
import           Servant.Server            (Context ((:.), EmptyContext),
                                            HasServer (ServerT),
                                            ServantErr (errBody), err400,
                                            err403, err404, err500, throwError)
import           Snap.Core                 (Snap, getHeader, getRequest)
import           Text.Show                 (Show (show))

import           AppData                   (DbAction, EnvApplication (..),
                                            Handler)
import           Auth                      (UserInfo (..), mkClaims,
                                            mkCompactJWT, verifyCompactJWT, getClearances)
import           Common.Api                    (RoutesAuth)
import           Common.Auth               (LoginData (..), SessionData (..),
                                            UserNew (..))
import Database (runDb)
import           Common.Model                     (Event (..),
                                            Journal (..), Rank (RankModerator),
                                            Subject (..))
import           DbAdapter                 (Alias (..), AuthPwd (..),
                                            Clearance (..), EntityField (..),
                                            EventSource (..), Unique (..),
                                            User (..))
import qualified DbAdapter                 as Db

default(Text)

handlers :: ServerT RoutesAuth '[] Handler
handlers =
         handleGrantAuthPwd
    :<|> handleUserNew
    :<|> handleDoesUserExist


handleGrantAuthPwd :: LoginData -> Handler (Maybe SessionData)
handleGrantAuthPwd LoginData{..} = do
  mUser <- runDb (getBy $ UUserName ldUserName)
  Entity keyUser User{..} <- maybe
    (throwError $ err400 { errBody = "user does not exist" })
    pure mUser
  mAuth <- runDb (getBy $ UAuthPwdFkUser keyUser)
  Entity _ AuthPwd{..} <- maybe
    (throwError $ err400 { errBody = "not registered with password"})
    pure mAuth
  Entity keyAlias alias <- case userFkDefaultAlias of
    Just key -> runDb (get key) >>= \case
      Just alias -> pure $ Entity key alias
      Nothing    -> throwError $ err500 { errBody = "alias not found" }
    Nothing  -> runDb (getWhere AliasFkUser keyUser) >>= \case
      [alias] -> pure alias
      _       -> throwError $ err500 { errBody = "alias not found" }
  case checkPassword (mkPassword ldPassword) authPwdPassword of
    PasswordCheckSuccess -> do
      now <- liftIO getCurrentTime
      let journalCreated = now
          journalEvent = EventLogin
          journalDescription = ""
          journalSubject = SubjectUser
          blob = Lazy.toStrict $ encode Journal{..}
      runDb $ insert_ $ Db.Journal blob userFkEventSource $ Just keyAlias
      let claims = mkClaims now ldUserName
      jwk <- asks envJwk
      let toServerError e = throwError $ err500 { errBody = BSU.fromString $ show e}
      sdJwt <- liftIO (runExceptT $ mkCompactJWT jwk claims)
        >>= either toServerError pure
      sdClearances <- getClearances keyAlias
      let sdIsSiteAdmin = userIsSiteAdmin
          sdUserName = userName
          sdAliasName = aliasName alias
      pure $ Just SessionData{..}
    PasswordCheckFail    -> pure Nothing

handleUserNew :: UserNew -> Handler SessionData
handleUserNew UserNew{..} = do
  when (Text.null unPassword) $
    throwError $ err400 { errBody = "Password cannot be empty" }
  when (Text.length unUserName > 64) $
    throwError $ err400 { errBody = "User name max length: 64 characters" }
  when (Text.length unPassword > 64) $
    throwError $ err400 { errBody = "Password max length: 64 characters" }
  unless (Text.all isAlphaNum unUserName) $
    throwError $ err400 { errBody = "user name may only contain alpha-numeric characaters" }
  sdIsSiteAdmin <- runDb $ (null :: [Entity User] -> Bool) <$> getAll
  eventSourceId <- runDb $ insert EventSource
  user <- runDb $ insert $ User unUserName sdIsSiteAdmin eventSourceId Nothing
  password <- hashPassword (mkPassword unPassword)
  runDb $ insert_ $ AuthPwd user password
  let sdAliasName = Text.take 16 unUserName
  keyAlias <- runDb $ insert $ Alias sdAliasName user
  runDb $ update user [ UserFkDefaultAlias =. Just keyAlias ]
  now <- liftIO getCurrentTime
  let blobJournalUser =
        let journalCreated = now
            journalEvent = EventCreation
            journalDescription = ""
            journalSubject = SubjectUser
        in  Lazy.toStrict $ encode Journal{..}
  runDb $ insert_ $ Db.Journal blobJournalUser eventSourceId Nothing
  let blobJournalAlias =
        let journalCreated = now
            journalEvent = EventCreation
            journalDescription = ""
            journalSubject = SubjectAlias
        in  Lazy.toStrict $ encode Journal{..}
  runDb $ insert_ $ Db.Journal blobJournalAlias eventSourceId Nothing
  jwk <- asks envJwk
  let claims = mkClaims now unUserName
      toServerError e = throwError $ err500 { errBody = BSU.fromString $ show e}
  sdJwt <- liftIO (runExceptT $ mkCompactJWT jwk claims)
    >>= either toServerError pure
  sdClearances <- getClearances keyAlias
  let sdUserName = unUserName
  pure SessionData{..}

handleDoesUserExist :: Text -> Handler Bool
handleDoesUserExist str = runDb $ isJust <$> getBy (UUserName str)

-- checkClearance :: UserInfo -> Text -> Rank -> Handler ()
-- checkClearance UserInfo{..} theShow minRank =
--   unless uiIsSiteAdmin $ do
--     case Map.lookup theShow uiClearances of
--       Just rank -> when (rank < minRank) $ throwError err403
--       Nothing   -> throwError err403
--
-- handleAliasRename :: UserInfo -> Text -> Handler ()
-- handleAliasRename UserInfo{..} new = do
--   when (Text.length new > 16) $
--     throwError $ err400 { errBody = "alias max length is 16 characters" }
--   now <- liftIO getCurrentTime
--   runDb $ update uiKeyAlias [ AliasName =. new ]
--   eventSourceId <- runDb (getBy $ UUserName uiUserName) >>=
--     maybe (throwError $ err400 { errBody = "user not found"})
--           (pure . userFkEventSource . entityVal)
--   let journalCreated = now
--       journalEvent = EventEdit
--       journalDescription = "Old alias: " <> aliasName uiAlias
--       journalSubject = SubjectAlias
--       journalBlob = Lazy.toStrict $ encode Journal{..}
--   runDb $ insert_ $ Db.Journal journalBlob eventSourceId $ Just uiKeyAlias
--
-- handleAliasGetAll :: UserInfo -> Handler [Text]
-- handleAliasGetAll UserInfo{..} = do
--   mUser <- runDb $ getBy $ UUserName uiUserName
--   keyUser <- maybe (throwError $ err500 { errBody = "user not found" })
--                    (pure . entityKey)
--                    mUser
--   ls <- runDb $ getWhere AliasFkUser keyUser
--   pure $ aliasName . entityVal <$> ls
--
-- handleAliasSetDefault :: UserInfo -> Text -> Handler ()
-- handleAliasSetDefault UserInfo{..} aliasName = do
--   keyAlias <- runDb (getBy $ UAliasName aliasName)
--     >>= maybe (throwError $ err500 { errBody = "alias name not found" })
--               (pure . entityKey)
--   keyUser <- runDb (getBy $ UUserName uiUserName)
--     >>= maybe (throwError $ err500 { errBody = "user not found" })
--               (pure . entityKey)
--   runDb $ update keyUser [ UserFkDefaultAlias =. Just keyAlias ]
