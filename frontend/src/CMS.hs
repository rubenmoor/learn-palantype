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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module CMS (elCMS, elCMSContent)where

import           Client                         ( getAuthData
                                                , getCMS
                                                , request, getCacheInvalidationData, postClearCache
                                                )
import           Common.Auth                    ( SessionData(..) )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( (.)

                                                )
import           Control.Monad                  ( Monad )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader           ( MonadReader(ask) )
import           Data.Either                    ( Either(..) )
import           Data.Foldable                  ( Foldable(length) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( ($>)
                                                , (<$>)
                                                , (<&>)

                                                , void
                                                )
import           Data.Int                       ( Int )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( Maybe(..), fromMaybe
                                                )
import           Data.Semigroup                 ( Semigroup((<>)) )
import qualified Data.Text                     as Text
import           Data.Time                      ( defaultTimeLocale, UTCTime (..), Day (ModifiedJulianDay), secondsToDiffTime )
import qualified Data.Time.Format              as Time
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


                                                , dyn_
                                                , el

                                                , elAttr
                                                , elClass
                                                , fanEither
                                                , leftmost


                                                , text
                                                , widgetHold
                                                , widgetHold_, switchDyn, attachWith, EventWriter
                                                )
import           Shared                         ( iFa
                                                , iFa', loadingScreen
                                                )
import           State                          ( Env(..)
                                                , Navigation(..)
                                                , Session(..)
                                                , State(..), updateState
                                                )
import           Text.Pandoc.Definition         ( Pandoc )
import           TextShow                       ( TextShow(showt) )
import           Witherable                     ( catMaybes
                                                )
import Data.Tuple (fst, snd)
import Data.Eq (Eq((==)))
import Control.Monad.Fix (MonadFix)
import Control.Lens (set)
import Data.Generics.Product (HasField(field))
import Data.Monoid (Endo)
import Common.Model (UTCTimeInUrl(UTCTimeInUrl))
import Reflex.Dom.Pandoc (elPandoc, defaultConfig)


elCMSContent
    :: forall t (m :: * -> *)
     . ( DomBuilder t m
       , MonadHold t m
       )
    => Event t Pandoc
    -> m ()
elCMSContent ev =
    widgetHold_ elWaitingForCMS $ elPandoc defaultConfig <$> ev
  where
    elWaitingForCMS = elClass "span" "small italic" $
      text "waiting for content-management-system"

elCMS
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
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
        filename = navPageName <> ".md"
        dynLatest =
              fromMaybe (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0))
            . Map.lookup (navSystemLang, navTextLang, filename)
            . stCMSCacheInvalidationData
          <$> envDynState

    evRespCMS <- Client.request $
      Client.getCMS (constDyn $ Right navSystemLang)
                    (constDyn $ Right navTextLang  )
                    (constDyn $ Right filename     )
                    (Right . UTCTimeInUrl <$> dynLatest)
                    $ leftmost [evLoadedAndBuilt, void evSuccCMSCache]


    dynPair <- el "div" $ widgetHold (loadingScreen "" $> (Nothing, never)) $
      attachWith (\mt e -> (mt,) <$> e) (current dynLatest) evRespCMS <&> \case
        Left  str             -> text ("CMS error: " <> str) $> (Nothing, never)
        Right (latest, parts) -> elCMSMenu numParts latest parts

    let evRefresh = switchDyn $ snd <$> dynPair
    evRespCMSCache <-
      request $ getCacheInvalidationData evRefresh
    let (evFailCMSCache, evSuccCMSCache) = fanEither evRespCMSCache
    updateState $ evSuccCMSCache <&> \ci ->
      [ set (field @"stCMSCacheInvalidationData") ci ]
    widgetHold_ blank $ evFailCMSCache <&> \msg ->
      el "div" $ text $ "Could not get CMS cache invalidation data" <> showt msg

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
elCMSMenu numParts latest parts = do
    Env {..} <- ask
    let
        n = length parts

    mParts <- if n == numParts
      then pure $ Just parts
      else do
        text $ "CMS error: " <> showt n <> " out of " <> showt numParts <> " expected parts."
        pure Nothing

    evRefresh <- elClass "div" "small floatRight gray italic" $ do
      text $ "Last update "
        <> Text.pack (Time.formatTime defaultTimeLocale "%Y-%m-%d %l:%M%P" latest)
        <> " "
      domRefresh <- elAttr "span" ("class" =: "icon-link verySmall" <> "title" =: "Refresh") $ iFa' "fas fa-sync"

      let dynSession = stSession <$> envDynState
      dyn_ $ dynSession <&> whenIsAdmin do
          text " "

          domSyncServerAll <- elAttr "span" ("class" =: "icon-link verySmall" <> "title" =: "Clear server cache") $
            iFa' "fas fa-skull"

          let Navigation{..} = envNavigation
          evRespAll <- Client.request $ Client.postClearCache
            (Client.getAuthData <$> envDynState)
            (constDyn $ Right navSystemLang)
            (constDyn $ Right navTextLang  )
            (constDyn $ Right navPageName  )
            $ domEvent Click domSyncServerAll
          widgetHold_ blank $ evRespAll <&> elClass "span" "verySmall" . \case
            Left  _ -> iFa "red fas fa-times"
            Right _ -> iFa "green fas fa-check"

      pure $ domEvent Click domRefresh

    pure (mParts, evRefresh)

whenIsAdmin :: Monad m => m () -> Session -> m ()
whenIsAdmin action (SessionUser SessionData{..}) | sdIsSiteAdmin = action
whenIsAdmin _ _ = blank
