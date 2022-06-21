{-# LANGUAGE NoImplicitPrelude #-}

module AppData
  ( module AppData
  ) where

import           Control.Monad.Reader   (ReaderT)
import           Crypto.JWT             (JWK)
import           Data.Pool              (Pool)
import           Data.Text              (Text)
import           Database.Persist.MySQL (SqlBackend)
import           Snap.Core              (Snap)
import           System.IO              (IO)

data EnvApplication = EnvApplication
  { envPool     :: Pool SqlBackend
  , envUrl      :: Text
  , envJwk      :: JWK
  }

type Handler = ReaderT EnvApplication Snap
type DbAction = ReaderT SqlBackend IO
