{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Frontend where

import           Language.Javascript.JSaddle    ( liftJSM )
import           State                          ( Session(..)
                                                , State(..)
                                                , defaultState, GetLoadedAndBuilt
                                                )

import           Common.Route                   ( FrontendRoute(..)
                                                , FrontendRoute_AuthPages(..), FrontendRoute_AdminPages (..), FrontendRoute_TextLang (..)
                                                )
import           Control.Monad.Reader           ( ReaderT(runReaderT) )
import           Data.Function                  ( ($)
                                                , (.)
                                                , const
                                                , flip
                                                )
import           Data.Functor                   ( ($>)
                                                , (<$>)
                                                , (<&>)
                                                , void
                                                )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( Maybe(..)
                                                , fromMaybe
                                                )
import           Data.Semigroup                 ( (<>)
                                                , Endo(..)
                                                )
import           Data.Text                      ( Text )
import           Home                           ( elStages
                                                , landingPage
                                                , message
                                                )
import           Obelisk.Frontend               ( Frontend(..)
                                                , ObeliskWidget
                                                )
import           Obelisk.Generated.Static       ( static )
import           Obelisk.Route                  ( pattern (:/)
                                                , R
                                                )
import           Obelisk.Route.Frontend         ( RoutedT

                                                , mapRoutedT
                                                , subRoute_
                                                )
import qualified Palantype.DE.Keys             as DE
import qualified Palantype.EN.Keys             as EN
import           Reflex.Dom                     ( (=:)
                                                , PerformEvent(performEvent_)
                                                , PostBuild(getPostBuild)
                                                , Reflex(Event, updated)
                                                , attach
                                                , attachPromptlyDynWith
                                                , blank
                                                , constDyn
                                                , current
                                                , dyn_
                                                , el
                                                , elAttr
                                                , fanEither
                                                , fanMap
                                                , foldDyn
                                                , gate
                                                , holdDyn
                                                , holdUniqDyn
                                                , leftmost
                                                , prerender
                                                , prerender_
                                                , runEventWriterT
                                                , select
                                                , tailE
                                                , text
                                                , zipDyn, mergeWith, mapAccumMaybeDyn, headE, delay
                                                )

import qualified AdminPages
import qualified AuthPages
import           Client                         ( getAppState
                                                , postAppState
                                                , request, getCacheInvalidationData, getLocallyCreateMissingFiles, getAuthData
                                                )
import           Common.Auth                    ( SessionData(..) )
import           Common.Model                   ( AppState
                                                , Rank(RankAdmin)
                                                , defaultAppState
                                                )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( (<<<) )
import           Control.Monad                  ( Monad((>>=)), (=<<) )
import           Data.Bool                      ( Bool(..)
                                                , not
                                                , (||), (&&)
                                                )
import           Data.Either                    ( Either(..) )
import           Data.Eq                        ( (==)
                                                , Eq
                                                )
import           Data.Functor.Misc              ( Const2(Const2) )
import           Data.Ord                       ( Ord((>=)) )
import qualified LocalStorage                  as LS
import           Palantype.Common.TH            ( failure )
import           Shared                         ( elLoading
                                                , redirectToWikipedia
                                                , requestPostViewPage, setRouteAndLoading
                                                )
import           Witherable                     ( Filterable(catMaybes, filter)
                                                )
import Data.Generics.Product (field)
import Control.Lens (set)

default(Text)

data FanAdmin
  = FanAdminLogin
  | FanAdminForbidden
  | FanAdminAccess
  deriving (Eq, Ord)

data FrontendLoaded = FrontendLoaded
  { flSession :: Bool
  , flCacheInvalidation :: Bool
  }

allLoaded :: FrontendLoaded -> Bool
allLoaded FrontendLoaded{..} = flSession && flCacheInvalidation

setSessionLoaded :: FrontendLoaded -> FrontendLoaded
setSessionLoaded fl = fl { flSession = True }

setCacheInvalidationLoaded :: FrontendLoaded -> FrontendLoaded
setCacheInvalidationLoaded fl = fl { flCacheInvalidation = True }

frontendAllLoaded :: FrontendLoaded -> Bool
frontendAllLoaded FrontendLoaded{..} = flSession && flCacheInvalidation

frontend :: Frontend (R FrontendRoute)
frontend =
    Frontend { _frontend_head = frontendHead, _frontend_body = frontendBody }

frontendBody
    :: forall t (m :: * -> *)
     . (ObeliskWidget t (R FrontendRoute) m)
    => RoutedT t (R FrontendRoute) m ()
frontendBody = mdo
    (evSessionInitial :: Event t (Maybe (Session, AppState))) <-
      updated <$> prerender (pure $ $failure "unexpected") do
            mSession <- liftJSM $ LS.retrieve LS.KeySession
            appState <- Data.Maybe.fromMaybe defaultAppState <$> liftJSM (LS.retrieve LS.KeyAppState)
            pure $ (,appState) <$> mSession

    let evMaybeLocal = evSessionInitial <&> \case
          Just (SessionUser _, _       ) -> Nothing
          Just (SessionAnon  , appState) -> Just (Just appState)
          Nothing                        -> Just Nothing

        evMaybeFromServer = evSessionInitial <&> \case
          Just (SessionUser sd, _) -> Just sd
          Just (SessionAnon   , _) -> Nothing
          Nothing                  -> Nothing

        evSessionFromServer = catMaybes evMaybeFromServer

    dynSessionData <- holdDyn Nothing $ Just <$> evSessionFromServer

    let dynAuthData = dynSessionData <&> \case
          Nothing -> Left "not logged in"
          Just SessionData{..} -> Right (sdJwt, sdAliasName)

    evRespServerSession <-
      request $ getAppState dynAuthData $ void evSessionFromServer

    let (evAppStateInvalid, evAppState) = fanEither evRespServerSession

    -- if we cannot get the app state, something is fishy with this session: reset
    prerender_ blank $ performEvent_ $ evAppStateInvalid $>
      liftJSM (LS.put LS.KeySession SessionAnon)

    let evSessionLocal = leftmost
          [ catMaybes evMaybeLocal
          , evAppStateInvalid $> Nothing
          ]

        (evLoadedFromServer :: Event t (Endo State)) =
           attach (current dynSessionData) evAppState <&> \case
             (Just sd, stApp) -> Endo $ \st -> st
                { stApp = stApp
                , stRedirectUrl = FrontendRoute_Main :/ ()
                , stSession = SessionUser sd
                }
             (Nothing, _) -> $failure "impossible"

        evLoadedLocal = evSessionLocal <&> \mAppState -> Endo $ \st -> st
            { stApp         = Data.Maybe.fromMaybe defaultAppState mAppState
            , stRedirectUrl = FrontendRoute_Main :/ ()
            , stSession     = SessionAnon
            }

    let evSessionLoaded = leftmost
            [ evLoadedFromServer
            , evLoadedLocal
            ]

    evRespCacheInvalidationData <-
      request $ getCacheInvalidationData $ void evSessionInitial

    let
        (_, evCacheInvalidationData) = fanEither evRespCacheInvalidationData
        evSetCacheInvalidationData =
          evCacheInvalidationData <&> Endo . set (field @"stCMSCacheInvalidationData")

        accFunc fl func =
          let fl' = func fl
          in  (Just fl', if frontendAllLoaded fl' then Just () else Nothing)
    (dynFrontendLoaded, evLoaded) <-
      mapAccumMaybeDyn accFunc (FrontendLoaded False False) $ mergeWith (.)
        [ evSessionLoaded         $> setSessionLoaded
        , evCacheInvalidationData $> setCacheInvalidationLoaded
        ]

    dynHasLoaded <- holdUniqDyn $ frontendAllLoaded <$> dynFrontendLoaded
    let
        getLoadedAndBuilt :: GetLoadedAndBuilt t
        getLoadedAndBuilt = do
          evPb <- delay 0 =<< getPostBuild
          headE $ leftmost [gate (current dynHasLoaded) evPb, evLoaded]

    -- TODO remove
    dyn_ $ dynFrontendLoaded <&> \fl@FrontendLoaded{..} ->
      if frontendAllLoaded fl
      then blank
      else
        let strLoadingSession = if not flSession
              then "Loading session ..."
              else ""
            strLoadingCacheInvalidation = if not flCacheInvalidation
              then "Loading cache invalidation data ...\n"
              else ""
        in  elLoading $ strLoadingSession <> "\n" <> strLoadingCacheInvalidation

    dynState <- foldDyn appEndo defaultState $ mergeWith (<>)
      [ evSessionLoaded
      , evStateUpdate
      , evSetCacheInvalidationData
      ]

    let isLoggedIn = \State{..} -> case stSession of
          SessionUser _ -> True
          SessionAnon   -> False
        dynEAuth = dynState <&> \State{..} -> case stSession of
          SessionAnon -> Left "not logged in"
          SessionUser SessionData{..} -> Right (sdJwt, sdAliasName)

    -- TODO: persist application state on visibility change (when hidden)
    updatedTail <- tailE $ updated dynState
    _ <- request $ postAppState dynEAuth (Right . stApp <$> dynState) $
        void $ filter isLoggedIn updatedTail

    prerender_ blank $ performEvent_ $
      filter (not <<< isLoggedIn) (updated dynState) <&> \State{..} ->
        liftJSM $ LS.put LS.KeyAppState stApp

    prerender_ blank $ performEvent_ $ updated dynState <&> \State{..} -> do
        liftJSM $ LS.put LS.KeySession stSession

    (_, evStateUpdate) <- mapRoutedT (runEventWriterT <<< flip runReaderT dynState) do

        message
        subRoute_ \case
            FrontendRoute_Main -> do
              getLoadedAndBuilt >>= requestPostViewPage (constDyn $ FrontendRoute_Main :/ ())
              landingPage
            FrontendRoute_SystemEN -> subRoute_ \case
                FrontendRoute_TextEN -> elStages @EN.Key getLoadedAndBuilt
            FrontendRoute_SystemDE -> subRoute_ \case
                FrontendRoute_TextEN -> elStages @DE.Key getLoadedAndBuilt
            FrontendRoute_Auth -> subRoute_ \case
                AuthPage_SignUp -> do
                  getLoadedAndBuilt >>= requestPostViewPage (constDyn $ FrontendRoute_Auth :/ AuthPage_SignUp :/ ())
                  AuthPages.signup
                AuthPage_Login  -> do
                  getLoadedAndBuilt >>= requestPostViewPage (constDyn $ FrontendRoute_Auth :/ AuthPage_Login :/ ())
                  AuthPages.login
                AuthPage_Settings -> do
                  dynSettings <- holdUniqDyn $ zipDyn dynHasLoaded (isLoggedIn <$> dynState) <&> \case
                    (False, _        ) -> Nothing
                    (True , bLoggedIn) -> Just bLoggedIn
                  let evToLogin    = filter (== Just False) $ updated dynSettings
                      evToSettings = filter (== Just True) $ updated dynSettings
                  setRouteAndLoading $ evToLogin $> FrontendRoute_Auth :/ AuthPage_Login :/ ()

                  requestPostViewPage (constDyn $ FrontendRoute_Auth :/ AuthPage_Settings :/ ()) $ void evToSettings
                  dyn_ $ dynSettings <&> \case
                    Just True  -> AuthPages.settings getLoadedAndBuilt
                    Just False -> blank
                    Nothing    -> blank
            FrontendRoute_Admin -> subRoute_ \case
                AdminPage_CreateMissingFiles -> do
                  evLoadedAndBuilt <- getLoadedAndBuilt
                  void $ request $ getLocallyCreateMissingFiles (getAuthData <$> dynState) evLoadedAndBuilt
                AdminPage_Journal -> do
                  evLoadedAndBuilt <- getLoadedAndBuilt
                  let
                      evAdmin = attachPromptlyDynWith const dynState evLoadedAndBuilt
                      selector = fanMap $ evAdmin <&> \State{..} -> case stSession of
                        SessionAnon -> Map.singleton FanAdminLogin ()
                        SessionUser SessionData{..} ->
                          if sdIsSiteAdmin || sdClearances >= RankAdmin
                          then Map.singleton FanAdminAccess ()
                          else Map.singleton FanAdminForbidden ()
                      evToLogin = select selector $ Const2 FanAdminLogin
                      evAccess = select selector $ Const2 FanAdminAccess
                      evForbidden = select selector $ Const2 FanAdminForbidden

                  setRouteAndLoading $ evToLogin $> FrontendRoute_Auth :/ AuthPage_Login :/ ()
                  prerender_ blank $ performEvent_ $ evForbidden $> redirectToWikipedia "HTTP_403"

                  requestPostViewPage (constDyn $ FrontendRoute_Admin :/ AdminPage_Journal :/ ()) evAccess
                  AdminPages.journal dynHasLoaded
    blank

frontendHead
    :: forall t (m :: * -> *)
     . (ObeliskWidget t (R FrontendRoute) m)
    => RoutedT t (R FrontendRoute) m ()
frontendHead = do
    el "title" $ text "Palantype"
    elAttr
        "link"
        (  "href"
        =: $(static "styles.css")
        <> "type"
        =: "text/css"
        <> "rel"
        =: "stylesheet"
        )
        blank
    elAttr
        "link"
        ("rel" =: "preconnect" <> "href" =: "https://fonts.googleapis.com")
        blank
    elAttr
        "link"
        (  "rel"
        =: "preconnect"
        <> "href"
        =: "https://fonts.gstatic.com"
        <> "crossorigin"
        =: "crossorigin"
        )
        blank
    elAttr
        "link"
        (  "href"
        =: "https://fonts.googleapis.com/css2?family=Abel&display=swap"
        <> "rel"
        =: "stylesheet"
        )
        blank
    elAttr
        "link"
        (  "href"
        =: "https://fonts.googleapis.com/css2?family=Special+Elite&display=swap"
        <> "rel"
        =: "stylesheet"
        )
        blank
    elAttr
        "link"
        (  "rel"
        =: "stylesheet"
        <> "href"
        =: $(static "FontAwesome/css/all.min.css") -- FontAwesome 5.15.1
        )
        blank
    elAttr
        "link"
        (  "rel"
        =: "stylesheet"
        <> "href"
        =: $(static "flag-icons-main/css/flag-icons.min.css")
        )
        blank
