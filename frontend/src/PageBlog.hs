{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module PageBlog
  ( pageBlog
  ) where

import Data.Tuple (fst, snd)
import qualified Data.Map.Strict               as Map
import           Control.Monad.Fix              ( MonadFix )
import Reflex.Dom
    ( (.~),
      Reflex(Event, current, never),
      Dynamic,
      attachWith,
      constDyn,
      fanEither,
      leftmost,
      holdUniqDyn,
      switchDyn,
      (=:),
      blank,
      dyn_,
      el,
      elAttr,
      elClass,
      text,
      updated,
      widgetHold,
      widgetHold_,
      MonadHold,
      PerformEvent(Performable),
      PostBuild,
      TriggerEvent,
      DomBuilder,
      HasDomEvent(domEvent),
      EventName(Click),
      Prerender, EventWriter )
import State (State (..), updateState, Loading (..), Session (..), GetLoadedAndBuilt)
import Control.Monad.Reader (MonadReader(ask), void)
import Control.Monad.IO.Class (MonadIO)
import Data.Time (UTCTime (..), Day (..), secondsToDiffTime, defaultTimeLocale)
import qualified Data.Time.Format as Time
import qualified Data.Text as Text
import Data.Maybe (fromMaybe, Maybe (..))
import Palantype.Common (SystemLang(..))
import Common.Model (TextLang(..), UTCTimeInUrl (..))
import Client (request, getCMSBlog, getCacheInvalidationData, getAuthData, postClearCache)
import Data.Functor (($>), (<$>))
import Control.Lens ( set, (<&>) )
import Data.Generics.Product ( HasField(field) )
import TextShow (TextShow(..))
import Data.Either (isRight, Either (..), either)
import Witherable (Filterable(..))
import Control.Category ( Category((.)) )
import Data.Function (($), const)
import Control.Applicative (Applicative(..))
import Data.Semigroup (Semigroup(..), Endo)
import Shared (iFa, iFa', elRouteLink)
import Control.Monad (Monad, join)
import Common.Auth ( SessionData(..) )
import Reflex.Dom.Pandoc (elPandoc, defaultConfig, elPandocInlines)
import Text.Pandoc.Definition (Pandoc(..), docTitle, docDate, docAuthors)
import Common.Route (FrontendRoute(FrontendRoute_Main))
import Obelisk.Route.Frontend         ( pattern (:/), SetRoute, R, RouteToUrl)
import Data.Foldable (for_)

pageBlog
  :: forall (m :: * -> *) t
  . ( DomBuilder t m
    , EventWriter t (Endo State) m
    , MonadFix m
    , MonadHold t m
    , MonadIO (Performable m)
    , MonadReader (Dynamic t State) m
    , PerformEvent t m
    , PostBuild t m
    , Prerender t m
    , RouteToUrl (R FrontendRoute) m
    , SetRoute t (R FrontendRoute) m
    , TriggerEvent t m
    )
  => GetLoadedAndBuilt t
  -> m ()
pageBlog getLoadedAndBuilt = elContainer mdo

    dynState <- ask
    evLoadedAndBuilt <- getLoadedAndBuilt

    let
        dynLatest =
              fromMaybe (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0))
            . Map.lookup (SystemDE, TextDE, "blog")
            . stCMSCacheInvalidationData
          <$> dynState

    evRespCMS <- request $
      getCMSBlog (Right . UTCTimeInUrl <$> dynLatest)
        $ leftmost [evLoadedAndBuilt, void evSuccCMSCache]

    dynPair <- el "div" $ widgetHold (pure ([], never)) $
      attachWith (\t e -> (t,) <$> e) (current dynLatest) evRespCMS <&> \case
        Left  str           -> text ("CMS error: " <> str) $> ([], never)
        Right (latest, doc) -> (doc,) <$> elCMSMenu latest

    let evRefresh = switchDyn $ snd <$> dynPair
    evRespCMSCache <- request $ getCacheInvalidationData $ void evRefresh
    let (evFailCMSCache, evSuccCMSCache) = fanEither evRespCMSCache

    updateState $ evSuccCMSCache <&> \ci ->
      [ set (field @"stCMSCacheInvalidationData") ci
      ]

    widgetHold_ blank $ evFailCMSCache <&> \msg ->
      el "div" $ text $ "Could not get CMS cache invalidation data" <> showt msg

    updateState $ evRefresh $>
      [ field @"stLoading" .~ LoadingStill "Retrieving CMS content"
      ]
    updateState $ filter isRight evRespCMS $>
      [ field @"stLoading" .~ LoadingDone
      ]
    updateState $ mapMaybe (either Just $ const Nothing) evRespCMS <&> \str ->
      [ field @"stLoading" .~ LoadingError str
      ]

    let
        elWaitingForCMS = elClass "span" "text-xs italic" $
          text "waiting for content-management-system"
        evContent = updated (fst <$> dynPair)

    widgetHold_ elWaitingForCMS $ evContent <&> \lsPandoc ->
      elClass "div" "my-prose" $ for_ lsPandoc \pandoc@(Pandoc meta _) -> do
        el "div" do
          elClass "span" "font-bold text-lg" $ elPandocInlines $ docDate meta
          el "span" do
            text " by "
            elPandocInlines $ join $ docAuthors meta
        el "br" blank
        el "h1" $ elPandocInlines $ docTitle meta
        elPandoc defaultConfig pandoc
        el "hr" blank
  where
    elContainer h = do
      elClass "div" "w-full h-28 pt-8 px-8 text-6xl bg-grayishblue-300 \
                    \text-grayishblue-900" do
        elRouteLink (FrontendRoute_Main :/ ()) $
          elClass "span" "float-left font-serif" $ text "Palantype DE"
        elClass "span" "float-right" $ text "Blog"
        elClass "br" "clear-both" blank
      elClass "div" "p-8" h


elCMSMenu
  :: forall (m :: * -> *) t
   . ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , MonadReader (Dynamic t State) m
     , PostBuild t m
     , Prerender t m
     )
  => UTCTime
  -> m (Event t ())
elCMSMenu latest = elClass "div" "text-xs float-right text-zinc-500 italic" do
    dynState <- ask
    elAttr "span" (  "class" =: "pr-1"
                  <> "title" =: "Latest update"
                  )
      $ text $ Text.pack (Time.formatTime defaultTimeLocale "%Y-%m-%d %l:%M%P" latest) <> " "

    domRefresh <- elAttr "span"
      (  "class" =: "cursor-pointer hover:text-grayishblue-800 mx-1"
      <> "title" =: "Refresh contents"
      ) $ iFa' "fas fa-sync"

    elAttr "a" (  "href" =: "https://github.com/rubenmoor/learn-palantype/blob/main/cms-content/blog/"
               <> "title" =: "Edit on Github"
               <> "class" =: "text-zinc-500 hover:text-grayishblue-800 mx-1 cursor-pointer"
               ) $ iFa "fas fa-edit"

    let dynSession = stSession <$> dynState
    dyn_ $ dynSession <&> whenIsAdmin do

        domSyncServerAll <- elAttr "span"
          (  "class" =: "cursor-pointer hover:text-grayishblue-800 mx-1"
          <> "title" =: "Clear server cache"
          ) $ iFa' "fas fa-skull"

        dynAuthData <- holdUniqDyn $ getAuthData <$> dynState
        evRespAll <- request $ postClearCache
          dynAuthData
          (constDyn $ Right SystemDE)
          (constDyn $ Right TextDE)
          (constDyn $ Right "blog"  )
          $ domEvent Click domSyncServerAll
        widgetHold_ blank $ evRespAll <&> \case
          Left  _ -> iFa "text-red-500 fas fa-times"
          Right _ -> iFa "text-green-500 fas fa-check"

    pure $ domEvent Click domRefresh

whenIsAdmin :: Monad m => m () -> Session -> m ()
whenIsAdmin action (SessionUser SessionData{..}) | sdIsSiteAdmin = action
whenIsAdmin _ _ = blank