{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module CMS where

import           Client                         ( getCMS
                                                , postRender
                                                , request, getAuthData, postClearCache, postClearCacheAll
                                                )
import           Common.Model                   ( TextLang(TextEN) )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( (<<<), (.) )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader           ( MonadReader(ask) )
import           Data.Either                    ( Either(..) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>)
                                                , (<&>)
                                                , Functor(fmap, (<$))
                                                , void, ($>)
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( Maybe(..)
                                                , fromMaybe
                                                , isNothing
                                                )
import           Data.Ord                       ( Ord((>)) )
import           Data.Semigroup                 ( Semigroup((<>)) )
import           Data.Time                      ( defaultTimeLocale )
import           Language.Javascript.JSaddle    ( liftJSM, eval )
import qualified LocalStorage                  as LS
import           Reflex.Dom                     ( DomBuilder
                                                , MonadHold
                                                , PerformEvent(..)
                                                , PostBuild (..)
                                                , Prerender (..)
                                                , Reflex(..)
                                                , blank
                                                , constDyn
                                                , current
                                                , el
                                                , fanEither
                                                , leftmost
                                                , prerender_
                                                , tag
                                                , text
                                                , widgetHold
                                                , widgetHold_, TriggerEvent, dyn, HasDomEvent (..), el', EventName (..), elClass, elAttr, (=:), dyn_, elDynClass
                                                )
import           State                          ( Env(..)
                                                , Navigation(..)
                                                , State
                                                    (..
                                                    ), Session (..)
                                                )
import           Text.Pandoc.Definition         ( Pandoc )
import           Witherable                     ( Filterable(filter)
                                                , catMaybes
                                                )
import Data.Foldable (Foldable(length))
import Data.Eq (Eq((==)))
import TextShow (TextShow(showt))
import Data.Int (Int)
import Control.Monad (Monad)
import Common.Auth (SessionData(..))
import Shared (dynSimple, iFa', iFa)
import qualified Data.Text as Text
import qualified Data.Time.Format as Time
import Data.Text (Text)

elCMS
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , MonadHold t m
       , MonadIO (Performable m)
       , MonadReader (Env t key) m
       , PerformEvent t m
       , Prerender t m
       , PostBuild t m
       , TriggerEvent t m
       )
    => Int
    -> m (Event t [Pandoc])
elCMS numParts = do
    Env {..} <- ask

    evLoadedAndBuilt <- envGetLoadedAndBuilt

    let
        Navigation {..} = envNavigation
        cacheInvalidation = current $ stCMSCacheInvalidationData <$> envDynState
        cacheKey = (navSystemLang, navTextLang, navPageName)

    evMFromCache <- postRender $ performEvent $
      tag cacheInvalidation evLoadedAndBuilt <&> \mapCacheInvalidation -> do
        let mLatest = Map.lookup cacheKey mapCacheInvalidation
        mMap <- liftJSM $ LS.retrieve LS.KeyCMSCache
        pure do
          map <- mMap
          cacheEntry@(time, _) <- Map.lookup cacheKey map
          if Just time > mLatest
            then Just cacheEntry
            else Nothing

    let evNotFromCache = void $ filter isNothing evMFromCache
        evFromCache = catMaybes evMFromCache

    (evRespFail, evRespSucc) <- fmap fanEither $ request $
      getCMS (constDyn $ Right navSystemLang)
             (constDyn $ Right navTextLang  )
             (constDyn $ Right navPageName  )
             evNotFromCache

    widgetHold_ blank $ evRespFail <&> \strError ->
      el "span" $ text $ "CMS error: " <> strError

    dynMParts <- widgetHold (pure Nothing) $
      leftmost [Just <$> evRespSucc, Just <$> evFromCache, Nothing <$ evRespFail] <&> \case
        Just (timeUpdated, parts) -> do
          prerender_ blank $ do
            liftJSM $ LS.update LS.KeyCMSCache $
              Map.insert (navSystemLang, TextEN, navPageName) (timeUpdated, parts) <<< fromMaybe Map.empty
          elClass "div" "small floatRight gray italic" $ do
            text $ "Last update "
              <> Text.pack (Time.formatTime defaultTimeLocale "%Y-%m-%d" timeUpdated)
              <> " "
            domSyncLocal <- elAttr "span" ("class" =: "icon-link verySmall" <> "title" =: "Clear cache and reload") $ iFa' "fas fa-sync"
            clearLocalCache $ domEvent Click domSyncLocal

            let dynSession = stSession <$> envDynState
            dyn_ $ dynSession <&> whenIsAdmin do
                text " "

                domSyncServer <- elAttr "span" ("class" =: "icon-link verySmall" <> "title" =: "Clear server cache") $
                  iFa' "fas fa-server"
                evResp <- request $ postClearCache (getAuthData <$> envDynState)
                                         (constDyn $ Right navSystemLang)
                                         (constDyn $ Right navTextLang)
                                         (constDyn $ Right navPageName)
                                         $ domEvent Click domSyncServer
                widgetHold_ blank $ evResp <&> elClass "span" "verySmall" . \case
                  Left  _ -> iFa "red fas fa-times"
                  Right _ -> iFa "green fas fa-check"

                text " "

                domSyncServerAll <- elAttr "span" ("class" =: "icon-link verySmall" <> "title" =: "Clear server cache of all pages") $
                  iFa' "fas fa-skull"
                evRespAll <- request $ postClearCacheAll (getAuthData <$> envDynState)
                                                         $ domEvent Click domSyncServerAll
                widgetHold_ blank $ evRespAll <&> elClass "span" "verySmall" . \case
                  Left  _ -> iFa "red fas fa-times"
                  Right _ -> iFa "green fas fa-check"
          pure $ Just parts
        Nothing -> pure $ Just []

    fmap catMaybes $ dyn $ dynMParts <&> el "div" . \case
        Nothing                               -> Nothing <$ text "Loading content ..."
        Just parts | length parts == numParts -> pure $ Just parts
        Just parts                            -> do
          el "p" $ text $ "Wrong number of parts, expected "
                       <> showt numParts
                       <> ", got "
                       <> showt (length parts)
          evResp <- el "p" $ do
            text "Try to clear the cache and reload."
            let dynSession = stSession <$> envDynState
            dynSimple $ dynSession <&> \case
              SessionUser SessionData{..} | sdIsSiteAdmin -> do
                text " If that doesn't do the trick, "
                (elB, _) <- el' "a" $ text "clear the server cache"
                let evClickB = domEvent Click elB
                evResp <- request $ postClearCacheAll (getAuthData <$> envDynState) evClickB
                text " and refresh the page."
                pure evResp
              _ -> pure never
          pure Nothing

clearLocalCache
  :: forall t (m :: * -> *)
  . ( PerformEvent t (Client m)
    , Prerender t m
    , Monad m
    )
  => Event t () -> m ()
clearLocalCache ev =
  prerender_ blank $ performEvent_ $ ev $> liftJSM do
    LS.put LS.KeyCMSCache Map.empty
    void $ eval ("window.location.reload(true)" :: Text)

whenIsAdmin :: Monad m => m () -> Session -> m ()
whenIsAdmin action (SessionUser SessionData{..}) | sdIsSiteAdmin = action
whenIsAdmin _ _ = blank
