{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.Api where

import           Common.Auth                    ( AuthOptional
                                                , AuthRequired
                                                , LoginData
                                                , SessionData
                                                , UserNew
                                                )
import           Common.Model                   ( AppState
                                                , Journal
                                                , Stats
                                                , TextLang
                                                )
import           Common.PloverConfig            ( PloverSystemCfg )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( Text )
import           Data.Time                      ( Day
                                                , UTCTime
                                                )
import           Palantype.Common               ( StageIndex
                                                , SystemLang(..)
                                                )
import           Servant.API                    ( (:<|>)
                                                , (:>)
                                                , Capture
                                                , Delete
                                                , Get
                                                , JSON
                                                , PlainText
                                                , Post
                                                , QueryFlag
                                                , QueryParam
                                                , ReqBody
                                                )
import           Text.Pandoc.Definition         ( Pandoc )
import           Web.KeyCode                    ( Key )

type RoutesAuth =
           "login" :> ReqBody '[JSON] LoginData :> Post '[JSON] (Maybe (SessionData, AppState))
      :<|> "new"   :> ReqBody '[JSON] UserNew   :> Post '[JSON] SessionData
      :<|> "user"  :> "exists" :> ReqBody '[JSON] Text      :> Post '[JSON] Bool
      :<|> "alias" :> "exists" :> ReqBody '[JSON] Text      :> Post '[JSON] Bool

      -- logout doesn't do anything server-side apart from journaling
      :<|> AuthRequired "jwt" :> "logout" :> Post '[JSON] ()

type RoutesUser =
       "alias" :>
    (
           AuthRequired "jwt" :> "rename"       :> ReqBody '[JSON] Text :> Post '[JSON] Text
      :<|> AuthRequired "jwt" :> "get" :> "all" :> Get '[JSON] [Text]
      :<|> AuthRequired "jwt" :> "setDefault"   :> ReqBody '[JSON] Text :> Post '[JSON] ()
      :<|> AuthRequired "jwt" :> "visibility"   :> ReqBody '[JSON] Bool :> Post '[JSON] ()
    )
  :<|> "app" :>
    (
           AuthRequired "jwt" :> "get" :> Get '[JSON] AppState
      :<|> AuthRequired "jwt" :> "put" :> ReqBody '[JSON] AppState :> Post '[JSON] ()
    )

type RoutesPalantype =
          "config" :> "new" :> ReqBody '[PlainText] String :> Post '[JSON] (SystemLang, PloverSystemCfg)

type RoutesEvent =
       AuthOptional "jwt" :> "view-page"       :> ReqBody '[JSON] Text                 :> Post '[JSON] ()

type RoutesStats =
       AuthOptional "jwt" :> Capture "lang" SystemLang :> Capture "stageIndex" StageIndex :> Get '[JSON] [(Maybe Text, Stats)]
  :<|> AuthRequired "jwt" :> "start" :> Post '[JSON] ()
  :<|> AuthOptional "jwt" :> "completed" :> ReqBody '[JSON] (SystemLang, StageIndex, Stats) :> Post '[JSON] ()

type RouteStatsNew =
       AuthRequired "jwt" :> Capture "created" UTCTime :> Delete '[JSON] ()

-- admin routes

-- data GetJournalOptions = GetJournalOptions
--   { gjoDayStart :: Day
--   , gjoDayEnd :: Day
--   , gjoExludeAdmin :: Bool
--   , gjoFilterByVisitor :: Maybe Int
--   , gjoFilterByUser :: Maybe Text
--   , gjoFilterByAlias :: Maybe Text
--   }

type RoutesAdmin =
  AuthRequired "jwt" :> "journal" :> QueryParam "start" Day
                                  :> QueryParam "end" Day
                                  :> QueryFlag "exclude-admin"
                                  :> QueryParam "filter-by-visitor" Int
                                  :> QueryParam "filter-by-user" Text
                                  :> QueryParam "filter-by-alias" Text
                                  :> QueryFlag "filter-anonymous"
                                  :> Get '[JSON] [Journal]

type RoutesCMS = QueryParam "system"   SystemLang
              :> QueryParam "lang"     TextLang
              :> QueryParam "pagename" Text
              :> Get '[JSON] [Pandoc]

type RoutesApi = "api" :>
    (      RoutesPalantype
      :<|> "admin" :> RoutesAdmin
      :<|> "auth"  :> RoutesAuth
      :<|> "user"  :> RoutesUser
      :<|> "event" :> RoutesEvent
      :<|> "stats" :> RoutesStats
      :<|> "cms"   :> RoutesCMS
    )

--

showSymbol :: SystemLang -> Text
showSymbol = \case
  SystemEN -> "EN"
  SystemDE -> "DE"

instance ToJSON Key
instance FromJSON Key
