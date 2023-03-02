{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module DbAdapter where

import Data.Time (UTCTime)
import           Data.ByteString         (ByteString)
import           Data.Password.Argon2    (Argon2, PasswordHash)
import           Data.Password.Instances ()
import           Data.Text               (Text)
import           Database.Persist.TH     (mkEntityDefList, mkPersist,
                                          persistLowerCase, share, sqlSettings)
import DbAdapter.Instances ()
import qualified Common.Model as Model
import Common.Model (TextLang)
import Palantype.Common (StageRepr, SystemLang)

share [mkPersist sqlSettings, mkEntityDefList "entities"] [persistLowerCase|
User                             -- some real person
  name             Text
  UUserName name
  isSiteAdmin      Bool
  fkDefaultAlias   AliasId Maybe
  blobAppState     ByteString
Visitor
  ipAddress        Text
  UIpAddress ipAddress
Alias                            -- one of several identities
  name             Text
  fkUser           UserId
  isVisible        Bool
  lastEdited       UTCTime
  UAliasName name
Clearance
  fkAlias          AliasId
  rank             Model.Rank
AuthPwd
  fkUser           UserId
  password         (PasswordHash Argon2)
  UAuthPwdFkUser fkUser
Journal
  created          UTCTime
  blob             ByteString
  fkVisitor        VisitorId
  fkMAlias         AliasId Maybe
Stats
  fkAlias          AliasId
  created          UTCTime
  time             Double
  lang             Text
  stageRepr        StageRepr
  length           Int
  nErrors          Int
StageBegin
  fkAlias          AliasId
  created          UTCTime
  UFkAlias fkAlias
CMSCache
  systemLang       SystemLang
  textLang         TextLang
  pageName         Text
  blob             ByteString
  UPageContent systemLang textLang pageName
CMSCacheInvalidation
  time             UTCTime
  systemLang       SystemLang
  textLang         TextLang
  pageName         Text
  UPage systemLang textLang pageName
|]
