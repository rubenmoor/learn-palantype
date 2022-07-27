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

module Handler.User
    ( handlers
    )
where

import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               (Category((.)) )
import           Control.Monad                  ( Monad((>>=))
                                                , when
                                                )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>) )
import           Data.Maybe                     ( fromMaybe
                                                , Maybe(..)
                                                , maybe
                                                )
import           Data.Ord                       ((<),  Ord((>)) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Database.Gerippe               ( Entity(..)
                                                , PersistUniqueRead(getBy)
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

import           AppData                        ( Handler )
import           Auth                           ( UserInfo(..) )
import           Common.Api                     ( RoutesUser )
import qualified DbAdapter                     as Db
import           Common.Model                   (JournalEvent(EventUser)
                                                , EventUser(..)
                                                , AppState(..)
                                                )
import           Database                       (blobDecode, blobEncode,  runDb
                                                )

import qualified DbJournal
import           Database.Gerippe               ( PersistStoreRead(get) )
import Data.Time (getCurrentTime, diffUTCTime)
import GHC.Num (fromInteger, Num((*)))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bool (Bool, (&&), not)
import TextShow (TextShow(showt))

default(Text)

handlers :: ServerT RoutesUser a Handler
handlers =
        (      handleAliasRename
          :<|> handleAliasGetAll
          :<|> handleAliasSetDefault
          :<|> handleAliasSetVisibility
        )
   :<|> (      handleGetAppState
          :<|> handlePutAppState
        )

handleAliasRename :: UserInfo -> Text -> Handler Text
handleAliasRename UserInfo {..} new = do
    now <- liftIO getCurrentTime
    when (Text.length new > 16) $ throwError $ err400
        { errBody = "alias max length is 16 characters"
        }
    when (not uiIsSiteAdmin && diffUTCTime now (Db.aliasLastEdited uiAlias) < fromInteger (3 * 30 * 86400)) $
      throwError $ err400
        { errBody = "You can rename your alias only once every 90 days."
        }
    runDb $ update uiKeyAlias [Db.AliasName =. new, Db.AliasLastEdited =. now]
    DbJournal.insert (Just uiKeyAlias) $ EventUser $ EventEdit
        "alias"
        (Db.aliasName uiAlias)
        new
    pure new

handleAliasGetAll :: UserInfo -> Handler [Text]
handleAliasGetAll UserInfo {..} = do
    mUser   <- runDb $ getBy $ Db.UUserName uiUserName
    keyUser <- maybe (throwError $ err500 { errBody = "user not found" })
                     (pure . entityKey)
                     mUser
    ls <- runDb $ getWhere Db.AliasFkUser keyUser
    pure $ Db.aliasName . entityVal <$> ls

handleAliasSetDefault :: UserInfo -> Text -> Handler ()
handleAliasSetDefault UserInfo {..} aliasName = do
    keyAlias <- runDb (getBy $ Db.UAliasName aliasName) >>= maybe
        (throwError $ err500 { errBody = "alias name not found" })
        (pure . entityKey)
    Entity keyUser Db.User {..} <-
        runDb (getBy $ Db.UUserName uiUserName)
            >>= maybe (throwError $ err500 { errBody = "user not found" }) pure
    mOldDefaultAlias <- case userFkDefaultAlias of
        Just keyDefaultAlias -> runDb (get keyDefaultAlias)
        Nothing              -> pure Nothing
    runDb $ update keyUser [Db.UserFkDefaultAlias =. Just keyAlias]
    DbJournal.insert (Just uiKeyAlias) $ EventUser $ EventEdit
        "default alias"
        (fromMaybe "" $ Db.aliasName <$> mOldDefaultAlias)
        aliasName

handleAliasSetVisibility :: UserInfo -> Bool -> Handler ()
handleAliasSetVisibility UserInfo {..} bVisible = do
    runDb $ update uiKeyAlias [Db.AliasIsVisible =. bVisible]
    DbJournal.insert (Just uiKeyAlias) $ EventUser $ EventEdit
        "alias visibility"
        (showt $ Db.aliasIsVisible uiAlias)
        (showt bVisible)

handleGetAppState :: UserInfo -> Handler AppState
handleGetAppState UserInfo {..} = do
    Db.User {..} <- runDb (getBy $ Db.UUserName uiUserName) >>= maybe
        (throwError $ err500 { errBody = "user not found" })
        (pure . entityVal)
    blobDecode userBlobAppState

handlePutAppState :: UserInfo -> AppState -> Handler ()
handlePutAppState UserInfo {..} appState = do
    keyUser <- runDb (getBy $ Db.UUserName uiUserName) >>= maybe
        (throwError $ err500 { errBody = "user not found" })
        (pure . entityKey)
    runDb $ update keyUser [Db.UserBlobAppState =. blobEncode appState]
