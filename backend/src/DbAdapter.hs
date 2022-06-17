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

import           Data.ByteString         (ByteString)
import           Data.Password.Argon2    (Argon2, PasswordHash)
import           Data.Password.Instances ()
import           Data.Text               (Text)
import           Database.Persist.TH     (mkEntityDefList, mkPersist,
                                          persistLowerCase, share, sqlSettings)
import DbAdapter.Instances ()
import qualified Common.Model as Model

share [mkPersist sqlSettings, mkEntityDefList "entities"] [persistLowerCase|
User                             -- some real person
  name             Text
  UUserName name
  isSiteAdmin      Bool
  fkEventSource    EventSourceId
  fkDefaultAlias   AliasId Maybe
Alias                            -- one of several identities
  name             Text
  fkUser           UserId
  UAliasName name
Clearance
  fkAlias          AliasId
  rank             Model.Rank
AuthPwd
  fkUser           UserId
  password         (PasswordHash Argon2)
  UAuthPwdFkUser fkUser
Journal
  blob             ByteString
  fkEventSource    EventSourceId
  fkAlias          AliasId Maybe
EventSource
|]
