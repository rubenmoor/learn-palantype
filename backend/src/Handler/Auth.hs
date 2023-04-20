{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE LiberalTypeSynonyms  #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Handler.Auth
    ( handlers
    )
where

import           Control.Applicative            ( Applicative(pure) )
import           Control.Monad                  ( Monad((>>=))
                                                , unless
                                                , when
                                                )
import           Control.Monad.Except           ( runExceptT )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Reader           ( asks )
import           Data.Bool                      ( Bool(..) )
import qualified Data.ByteString.Lazy.UTF8     as BSU
import           Data.Char                      ( isAlphaNum )
import           Data.Either                    (either )
import           Data.Function                  (($) )
import Data.Functor ( (<$>), Functor(fmap) )
import           Data.List                      ( null )
import           Data.Maybe                     ( fromMaybe
                                                , Maybe(Just, Nothing)
                                                , isJust
                                                , maybe
                                                )
import           Data.Ord                       ( Ord((>)) )
import           Text.Show                      ( show )
import           Data.Password.Argon2           ( PasswordCheck(..)
                                                , checkPassword
                                                , hashPassword
                                                , mkPassword
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Time                      ( getCurrentTime )
import           Database.Gerippe               ( Entity(..)
                                                , PersistStoreWrite
                                                    ( insert
                                                    , insert_
                                                    )
                                                , PersistUniqueRead(getBy)
                                                , get
                                                , getAll
                                                , getWhere
                                                )
import           Database.Persist.MySQL         ( PersistStoreWrite(update)
                                                , (=.)
                                                )
import           Servant.API                    ( (:<|>)(..) )
import           Servant.Server                 ( HasServer(ServerT)
                                                , ServantErr(errBody)
                                                , err400
                                                , err500
                                                , throwError
                                                )

import           AppData                        ( EnvApplication(..)
                                                , Handler
                                                )
import           Auth                           (UserInfo (..),  mkClaims
                                                , mkCompactJWT
                                                , getClearances
                                                )
import           Common.Api                     ( RoutesAuth )
import           Common.Auth                    ( LoginData(..)
                                                , SessionData(..)
                                                , UserNew(..)
                                                )
import           Database                       (blobDecode, blobEncode,  runDb )
import           Common.Model                   (defaultAppState, Rank (..), JournalEvent(..), EventUser(..), AppState,          )
import qualified DbJournal
import qualified DbAdapter                     as Db
import Control.Category ((<<<))

default(Text)

handlers :: ServerT RoutesAuth a Handler
handlers =
       handleGrantAuthPwd
  :<|> handleUserNew
  :<|> handleDoesUserExist
  :<|> handleDoesAliasExist
  :<|> handleLogout

handleGrantAuthPwd :: LoginData -> Handler (Maybe (SessionData, AppState))
handleGrantAuthPwd LoginData {..} = runDb (getBy $ Db.UUserName ldUserName) >>= \case
    Nothing -> pure Nothing
    Just (Entity keyUser Db.User {..}) -> do
        mAuth                    <- runDb (getBy $ Db.UAuthPwdFkUser keyUser)
        Entity _ Db.AuthPwd {..} <- maybe
            (throwError $ err400 { errBody = "not registered with password" })
            pure
            mAuth
        Entity keyAlias alias <- case userFkDefaultAlias of
            Just key -> runDb (get key) >>= \case
                Just alias -> pure $ Entity key alias
                Nothing    -> throwError $ err500 { errBody = "alias not found" }
            Nothing -> runDb (getWhere Db.AliasFkUser keyUser) >>= \case
                [alias] -> pure alias
                _       -> throwError $ err500 { errBody = "alias not found" }
        case checkPassword (mkPassword ldPassword) authPwdPassword of
            PasswordCheckSuccess -> do
                now <- liftIO getCurrentTime
                let claims = mkClaims now ldUserName
                jwk <- asks envJwk
                let toServerError e = throwError
                        $ err500 { errBody = BSU.fromString $ show e }
                sdJwt <-
                    liftIO (runExceptT $ mkCompactJWT jwk claims)
                        >>= either toServerError pure
                sdClearances <- getClearances keyAlias
                let sdIsSiteAdmin = userIsSiteAdmin
                    sdUserName    = userName
                    sdAliasName   = Db.aliasName alias
                    sdAliasVisible = Db.aliasIsVisible alias
                    appState = fromMaybe defaultAppState $ blobDecode userBlobAppState

                DbJournal.insert (Just keyAlias) $ EventUser EventLogin
                pure $ Just (SessionData { .. }, appState)
            PasswordCheckFail -> pure Nothing

handleUserNew :: UserNew -> Handler SessionData
handleUserNew UserNew {..} = do
    when (Text.null unPassword) $ throwError $ err400
        { errBody = "Password cannot be empty"
        }
    when (Text.length unUserName > 64) $ throwError $ err400
        { errBody = "User name max length: 64 characters"
        }
    when (Text.length unPassword > 64) $ throwError $ err400
        { errBody = "Password max length: 64 characters"
        }
    unless (Text.all isAlphaNum unUserName) $ throwError $ err400
        { errBody = "user name may only contain alpha-numeric characters"
        }
    sdIsSiteAdmin <- runDb $ (null :: [Entity Db.User] -> Bool) <$> getAll
    user          <- runDb $ insert $ Db.User unUserName
                                              sdIsSiteAdmin
                                              Nothing
                                              (blobEncode unAppState)
    password <- hashPassword $ mkPassword unPassword
    runDb $ insert_ $ Db.AuthPwd user password
    let sdAliasName = fromMaybe (Text.take 16 unUserName) unMAlias
    now <- liftIO getCurrentTime
    keyAlias <- runDb $ insert $ Db.Alias sdAliasName user unVisible now
    runDb $ update user [Db.UserFkDefaultAlias =. Just keyAlias]

    DbJournal.insert (Just keyAlias) $ EventUser EventSignup

    jwk <- asks envJwk
    let claims = mkClaims now unUserName
        toServerError e =
            throwError $ err500 { errBody = BSU.fromString $ show e }
    sdJwt <-
        liftIO (runExceptT $ mkCompactJWT jwk claims)
            >>= either toServerError pure
    let sdClearances = if sdIsSiteAdmin then RankAdmin else RankMember
    runDb $ insert_ $ Db.Clearance keyAlias sdClearances
    let sdUserName = unUserName
        sdAliasVisible = unVisible
    pure SessionData { .. }

handleDoesUserExist :: Text -> Handler Bool
handleDoesUserExist = runDb <<< fmap isJust <<< getBy <<< Db.UUserName

handleDoesAliasExist :: Text -> Handler Bool
handleDoesAliasExist = runDb <<< fmap isJust <<< getBy <<< Db.UAliasName

handleLogout :: UserInfo -> Handler ()
handleLogout UserInfo{..} = DbJournal.insert (Just uiKeyAlias) $ EventUser EventLogout
