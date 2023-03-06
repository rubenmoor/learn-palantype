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
{-# LANGUAGE RecursiveDo #-}

module CMS where

import           Client                         ( getAuthData
                                                , getCMS

                                                , postClearCacheAll
                                                , postRender
                                                , request
                                                )
import           Common.Auth                    ( SessionData(..) )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( (.)
                                                , (<<<)
                                                )
import           Control.Monad                  ( Monad )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader           ( MonadReader(ask) )
import           Data.Bool                      ( Bool(..) )
import           Data.Either                    ( Either(..) )
import           Data.Foldable                  ( Foldable(length) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( ($>)
                                                , (<$>)
                                                , (<&>)
                                                , Functor(fmap)
                                                , void
                                                )
import           Data.Int                       ( Int )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( Maybe(..)
                                                , fromMaybe
                                                , isNothing
                                                )
import           Data.Ord                       ( Ord((>)) )
import           Data.Semigroup                 ( Semigroup((<>)) )
import qualified Data.Text                     as Text
import           Data.Time                      ( defaultTimeLocale )
import qualified Data.Time.Format              as Time
import           Language.Javascript.JSaddle    ( liftJSM
                                                )
import qualified LocalStorage                  as LS
import           Reflex.Dom                     ( (=:)
                                                , DomBuilder
                                                , EventName(..)
                                                , HasDomEvent(..)
                                                , MonadHold (..)
                                                , PerformEvent(..)
                                                , PostBuild(..)
                                                , Prerender(..)
                                                , Reflex(..)
                                                , TriggerEvent
                                                , blank
                                                , constDyn
                                                , current

                                                , dyn_
                                                , el

                                                , elAttr
                                                , elClass
                                                , fanEither
                                                , leftmost
                                                , prerender_
                                                , tag
                                                , text
                                                , widgetHold
                                                , widgetHold_, switchDyn
                                                )
import           Shared                         ( iFa
                                                , iFa'
                                                )
import           State                          ( Env(..)
                                                , Navigation(..)
                                                , Session(..)
                                                , State(..)
                                                )
import           Text.Pandoc.Definition         ( Pandoc )
import           TextShow                       ( TextShow(showt) )
import           Witherable                     ( Filterable(filter)
                                                , catMaybes
                                                )
import Data.Time.Clock (UTCTime)
import Data.Tuple (fst, snd)
import Data.Eq (Eq((==)))
import Control.Monad.Fix (MonadFix)

elCMS
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , MonadFix m
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
elCMS numParts = mdo
    Env {..} <- ask

    evLoadedAndBuilt <- envGetLoadedAndBuilt

    let
        Navigation {..} = envNavigation
        cacheInvalidation = current $ stCMSCacheInvalidationData <$> envDynState
        cacheKey = (navSystemLang, navTextLang, navPageName)

    evMFromCache <- Client.postRender $ performEvent $
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

    dynRefresh <- holdDyn False $ evRefresh $> True

    (evRespFail, evRespSucc) <- fmap fanEither $ Client.request $
      Client.getCMS (constDyn $ Right navSystemLang)
             (constDyn $ Right navTextLang  )
             (constDyn $ Right navPageName  )
             dynRefresh
             $ leftmost [evNotFromCache, evRefresh]

    widgetHold_ blank $ evRespFail <&> \strError ->
      el "span" $ text $ "CMS error: " <> strError

    dynPair <- el "div" $ widgetHold (text "loading content ..." $> (Nothing, never)) $
      leftmost [Right <$> evRespSucc, Right <$> evFromCache, Left <$> evRespFail] <&> \case
        Left str            -> text ("CMS error: " <> str) $> (Nothing, never)
        Right (time, parts) -> elCMSMenu numParts time parts

    let evRefresh = switchDyn $ snd <$> dynPair
    pure $ catMaybes $ updated $ fst <$> dynPair

elCMSMenu
    :: ( MonadReader (Env t key) m
       , DomBuilder t m
       , MonadHold t m
       , PostBuild t m
       , Prerender t m
       )
    => Int
    -> UTCTime
    -> [Pandoc]
    -> m (Maybe [Pandoc], Event t ())
elCMSMenu numParts time parts = do
    Env {..} <- ask
    let
        Navigation {..} = envNavigation
        n = length parts

    mParts <- if n == numParts
      then do
        prerender_ blank $ liftJSM $ LS.update LS.KeyCMSCache $
             Map.insert (navSystemLang, navTextLang, navPageName) (time, parts)
               <<< fromMaybe Map.empty
        pure $ Just parts
      else do
        text $ "CMS error: " <> showt n <> " out of " <> showt numParts <> "expected parts."
        pure Nothing

    evRefresh <- elClass "div" "small floatRight gray italic" $ do
      text $ "Last update "
        <> Text.pack (Time.formatTime defaultTimeLocale "%Y-%m-%d" time)
        <> " "
      domRefresh <- elAttr "span" ("class" =: "icon-link verySmall" <> "title" =: "Refresh") $ iFa' "fas fa-sync"

      let dynSession = stSession <$> envDynState
      dyn_ $ dynSession <&> whenIsAdmin do
          text " "

          domSyncServerAll <- elAttr "span" ("class" =: "icon-link verySmall" <> "title" =: "Clear server cache of all pages") $
            iFa' "fas fa-skull"
          evRespAll <- Client.request $ Client.postClearCacheAll (Client.getAuthData <$> envDynState)
                                                    $ domEvent Click domSyncServerAll
          widgetHold_ blank $ evRespAll <&> elClass "span" "verySmall" . \case
            Left  _ -> iFa "red fas fa-times"
            Right _ -> iFa "green fas fa-check"

      pure $ domEvent Click domRefresh

    pure (mParts, evRefresh)



whenIsAdmin :: Monad m => m () -> Session -> m ()
whenIsAdmin action (SessionUser SessionData{..}) | sdIsSiteAdmin = action
whenIsAdmin _ _ = blank
