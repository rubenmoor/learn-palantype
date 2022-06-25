{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.Api where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Map                       ( Map )
import           Data.Text                      ( Text )
import           Servant.API                    ( Capture
                                                , (:<|>)
                                                , Get
                                                , (:>)
                                                , JSON
                                                , PlainText
                                                , Post
                                                , ReqBody
                                                )
import           Web.KeyCode                    ( Key )
import           Palantype.Common               ( MapStenoWordTake100
                                                , PatternDoc
                                                , PatternPos
                                                , Greediness
                                                , Lang(..)
                                                )
import qualified Palantype.DE                  as DE
import           Palantype.Common               ( RawSteno )

import           Common.PloverConfig            ( PloverSystemCfg )
import           Common.Auth                    ( LoginData
                                                , SessionData
                                                , UserNew
                                                , AuthRequired
                                                , AuthOptional
                                                )
import           Common.Model (Stats, AppState)
import Common.Stage (Stage)

type RoutesAuth =
           "login"  :> ReqBody '[JSON] LoginData :> Post '[JSON] (Maybe (SessionData, AppState))
      :<|> "new"    :> ReqBody '[JSON] UserNew   :> Post '[JSON] SessionData
      :<|> "exists" :> ReqBody '[JSON] Text      :> Post '[JSON] Bool

      -- logout doesn't do anything server-side apart from journaling
      :<|> AuthRequired "jwt" :> "logout" :> Post '[JSON] ()

type RoutesUser =
       "alias" :>
    (
           AuthRequired "jwt" :> "rename"       :> ReqBody '[JSON] Text :> Post '[JSON] ()
      :<|> AuthRequired "jwt" :> "get" :> "all" :> Get '[JSON] [Text]
      :<|> AuthRequired "jwt" :> "setDefault"   :> ReqBody '[JSON] Text :> Post '[JSON] ()
    )
  :<|> "app" :>
    (
           AuthRequired "jwt" :> "get" :> Get '[JSON] AppState
      :<|> AuthRequired "jwt" :> "put" :> ReqBody '[JSON] AppState :> Post '[JSON] ()
    )

type RoutesPalantype =
          "config" :> "new" :> ReqBody '[PlainText] String :> Post '[JSON] (Lang, PloverSystemCfg)
     :<|> "doc"    :> "DE" :> "pattern" :> "all" :> Get '[JSON] (PatternDoc DE.Key, MapStenoWordTake100 DE.Key)
     :<|> "doc"    :> "DE" :> "pattern" :> Capture "pattern-group" DE.Pattern
                                        :> Capture "greediness" Greediness
                                        :> Get '[JSON] [(PatternPos, [(Text, RawSteno)])]
     :<|> "dict"   :> "DE" :> Capture "pattern-group" DE.Pattern
                           :> Capture "greediness" Greediness
                           :> Get '[JSON] (Map RawSteno Text, Map Text [RawSteno])
     :<|> "dict"   :> "DE" :> "Numbers"
                           :> Get '[JSON] (Map RawSteno Text)

type RoutesEvent =
       AuthOptional "jwt" :> "view-page"       :> ReqBody '[JSON] Text           :> Post '[JSON] ()
  :<|> AuthOptional "jwt" :> "stage-completed" :> ReqBody '[JSON] (Stage, Stats) :> Post '[JSON] ()

type RoutesApi = "api" :>
    (      RoutesPalantype
      :<|> "auth"  :> RoutesAuth
      :<|> "user"  :> RoutesUser
      :<|> "event" :> RoutesEvent
    )

--

showSymbol :: Lang -> Text
showSymbol = \case
  EN -> "EN"
  DE -> "DE"

instance ToJSON Key
instance FromJSON Key
