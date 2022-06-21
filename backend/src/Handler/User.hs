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
  ) where

import           Control.Applicative (Applicative (pure))
import           Control.Category (Category ((.)))
import           Control.Monad (Monad ((>>=)), when)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Function (($))
import           Data.Functor ((<$>))
import           Data.Maybe                (Maybe (Just),
                                            maybe)
import           Data.Monoid ((<>))
import           Data.Ord (Ord ((>)))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (getCurrentTime)
import           Database.Gerippe          (Entity (..),
                                            PersistStoreWrite (insert_),
                                            PersistUniqueRead (getBy),
                                            getWhere
                                            )
import           Database.Persist.MySQL    (PersistStoreWrite (update), (=.))
import           Servant.API               ((:<|>) (..))
import           Servant.Server            (HasServer (ServerT),
                                            ServantErr (errBody), err400,
                                            err500, throwError)
import           Snap.Core (Snap)

import           AppData                   (Handler)
import           Auth                      (UserInfo (..))
import           Common.Api (RoutesUser)
import           DbAdapter                 (Alias (..), EntityField (..), Unique (..),
                                            User (..))
import qualified DbAdapter as Db
import           Common.Model                     (Event (..),
                                            Journal (..),
                                            Subject (..) )
import           Database (runDb)

default(Text)

handlers :: ServerT RoutesUser '[Snap UserInfo] Handler
handlers =
         handleAliasRename
    :<|> handleAliasGetAll
    :<|> handleAliasSetDefault

handleAliasRename :: UserInfo -> Text -> Handler ()
handleAliasRename UserInfo{..} new = do
  when (Text.length new > 16) $
    throwError $ err400 { errBody = "alias max length is 16 characters" }
  now <- liftIO getCurrentTime
  runDb $ update uiKeyAlias [ AliasName =. new ]
  eventSourceId <- runDb (getBy $ UUserName uiUserName) >>=
    maybe (throwError $ err400 { errBody = "user not found"})
          (pure . userFkEventSource . entityVal)
  let journalCreated = now
      journalEvent = EventEdit
      journalDescription = "Old alias: " <> aliasName uiAlias
      journalSubject = SubjectAlias
      journalBlob = Lazy.toStrict $ encode Journal{..}
  runDb $ insert_ $ Db.Journal journalBlob eventSourceId $ Just uiKeyAlias

handleAliasGetAll :: UserInfo -> Handler [Text]
handleAliasGetAll UserInfo{..} = do
  mUser <- runDb $ getBy $ UUserName uiUserName
  keyUser <- maybe (throwError $ err500 { errBody = "user not found" })
                   (pure . entityKey)
                   mUser
  ls <- runDb $ getWhere AliasFkUser keyUser
  pure $ aliasName . entityVal <$> ls

handleAliasSetDefault :: UserInfo -> Text -> Handler ()
handleAliasSetDefault UserInfo{..} aliasName = do
  keyAlias <- runDb (getBy $ UAliasName aliasName)
    >>= maybe (throwError $ err500 { errBody = "alias name not found" })
              (pure . entityKey)
  keyUser <- runDb (getBy $ UUserName uiUserName)
    >>= maybe (throwError $ err500 { errBody = "user not found" })
              (pure . entityKey)
  runDb $ update keyUser [ UserFkDefaultAlias =. Just keyAlias ]
