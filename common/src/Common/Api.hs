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
                                                , TextLang, UTCTimeInUrl
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
import Data.Map.Strict (Map)

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

type RoutesAdmin =
       AuthRequired "jwt" :> "journal"
           :> QueryParam "start" Day
           :> QueryParam "end" Day
           :> QueryFlag "exclude-admin"
           :> QueryParam "filter-by-visitor" Int
           :> QueryParam "filter-by-user" Text
           :> QueryParam "filter-by-alias" Text
           :> QueryFlag "filter-anonymous"
           :> Get '[JSON] [Journal]
  :<|> AuthRequired "jwt" :> "locally-create-missing-files" :> Get '[JSON] ()

Type RoutesCMS =
          Capture "system"   SystemLang
       :> Capture "lang"     TextLang
       :> Capture "pagename" Text
       -- :> Capture "time"     UTCTimeInUrl
       :> Capture "time"     UTCTime
       :> Get '[JSON] [Pandoc]

  -- Route to be called by github action
  :<|> "invalidate-cache"        :> ReqBody '[JSON] [Text] :> Post '[JSON] ()

  :<|> "cache-invalidation-data" :> Get '[JSON] (Map (SystemLang, TextLang, Text) UTCTime)

  :<|> AuthRequired "jwt" :> "clear-cache" :> "all" :> Post '[JSON] ()

  :<|> AuthRequired "jwt" :> "clear-cache"
        :> Capture "system"   SystemLang
        :> Capture "lang"     TextLang
        :> Capture "pagename" Text
        :> Capture "time"     UTCTime
        :> Post '[JSON] ()

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
