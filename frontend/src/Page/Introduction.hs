{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Page.Introduction where

import           Client                         ( getCMS
                                                , request, postRender
                                                )
import           Common.Model                   ( TextLang(TextEN) )
import           Common.Route                   ( FrontendRoute(..) )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( (.), (<<<) )
import           Control.Lens                   ( (%~)
                                                , (.~)
                                                )
import           Control.Monad.Reader           ( MonadReader(ask), MonadFix )
import           Data.Eq                        ( Eq((==)) )
import           Data.Foldable                  ( Foldable(length) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( ($>)
                                                , (<$>)
                                                , (<&>)
                                                , Functor(fmap)
                                                , void
                                                )
import           Data.Generics.Product          ( field )
import qualified Data.Map                      as Map
import           Data.Monoid                    ( Monoid(mempty, mconcat) )
import           Data.Semigroup                 ( Endo(..)
                                                , Semigroup((<>))
                                                )
import qualified Data.Set                      as Set
import           Obelisk.Route.Frontend         ( R
                                                , SetRoute(setRoute)
                                                )
import           Palantype.Common               ( Palantype
                                                , SystemLang(..)
                                                )
import           Palantype.Common.RawSteno      ( parseChordMaybe )
import           Palantype.Common.TH            ( fromJust )
import           Reflex.Dom                     ( (=:)
                                                , DomBuilder
                                                , Event
                                                , EventName(Click)
                                                , EventWriter
                                                , HasDomEvent(domEvent)
                                                , MonadHold
                                                , PostBuild
                                                , blank
                                                , constDyn
                                                , dyn_
                                                , el
                                                , elAttr
                                                , elClass
                                                , elClass'
                                                , fanEither
                                                , getPostBuild
                                                , leftmost
                                                , text
                                                , widgetHold
                                                , widgetHold_, prerender_, PerformEvent (performEvent), current, tag, Prerender
                                                )
import           Reflex.Dom.Pandoc              ( defaultConfig
                                                , elPandoc
                                                )
import           Shared                         ( loadingScreen )
import           State                          ( Env(..)
                                                , Navigation(..)
                                                , State (stCMSCacheInvalidationData)
                                                , stageUrl
                                                , updateState
                                                )
import           TextShow                       ( TextShow(showt) )
import Witherable ( Filterable(filter), catMaybes )
import Data.Either (Either(..))
import qualified LocalStorage                  as LS
import Language.Javascript.JSaddle (liftJSM)
import Data.Maybe (fromMaybe, isNothing, Maybe (..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Time (getCurrentTime)
import Data.Ord ((>))

introduction
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , Prerender t m
       , PostBuild t m
       , SetRoute t (R FrontendRoute) m
       )
    => (Event t () -> Event t ())
    -> m Navigation
introduction toReady = do

    Env {..} <- ask
    evReady <- toReady <$> getPostBuild

    let
        Navigation {..} = envNavigation
        behaviorCacheInvalidation = current $ stCMSCacheInvalidationData <$> envDynState
        cacheKey = (navLang, TextEN, "introduction")

    evMFromCache <- postRender $ performEvent $
      tag behaviorCacheInvalidation evReady <&> \mapCacheInvalidation -> do
        let latest = Map.lookup cacheKey mapCacheInvalidation
        mMap <- liftJSM $ LS.retrieve LS.KeyCMSCache
        pure do
          map <- mMap
          (time, contents) <- Map.lookup cacheKey map
          if Just time > latest
            then pure contents
            else Nothing

    let evNotFromCache = void $ filter isNothing evMFromCache
        evFromCache = catMaybes evMFromCache

    (evRespFail, evRespSucc) <- fmap fanEither $ request $
      getCMS (constDyn $ Right navLang       )
             (constDyn $ Right TextEN        )
             (constDyn $ Right "introduction")
             evNotFromCache

    widgetHold_ blank $ evFromCache $> el "div" (text "Retrieved contents from cache")
    widgetHold_ blank $ evRespSucc $> el "div" (text "Retrieved contents from webserver")

    widgetHold_ blank $ evRespFail <&> \strError ->
      el "span" $ text $ "CMS error: " <> strError

    dynParts <- widgetHold (loadingScreen "" $> (mempty, mempty, mempty)) $
      leftmost [evRespSucc, evFromCache] <&> \case
        parts@[p1, p2, p3] -> do
          prerender_ blank $ do
            now <- liftIO getCurrentTime
            liftJSM $ LS.update LS.KeyCMSCache $
              Map.insert (navLang, TextEN, "introduction") (now, parts) <<< fromMaybe Map.empty
          pure (p1, p2, p3)
        parts        -> do
          el "div" $ text $
                 "CMS error: wrong number of parts in markdown. Expected: 3. Got: "
              <> showt (length parts)
          pure (mconcat parts, mempty, mempty)

    dyn_ $ dynParts <&> \(part1, part2, part3) -> do
      el "h1" $ text "Introduction"

      elPandoc defaultConfig part1

      elClass "p" "textAlign-center" $ elAttr
          "iframe"
          (  "width"
          =: "640"
          <> "height"
          =: "480"
          <> "src"
          =: "https://www.youtube.com/embed/za1qxU4jdfg"
          )
          blank

      elPandoc defaultConfig part2

      let rsStart = case navLang of
              SystemEN -> "START"
              SystemDE -> "DSAÜD"
          eChordSTART =
              void $ filter (== $fromJust (parseChordMaybe rsStart)) envEChord

      elClass "div" "start" $ do
          (btn, _) <- elClass' "button" "small" $ text "Get Started!"
          let eStart = leftmost [eChordSTART, domEvent Click btn]
          updateState
              $  eStart
              $> [ field @"stApp" . field @"stProgress" %~ Map.insert navLang ($fromJust navMNext)
                 , field @"stApp" . field @"stCleared" %~ Set.insert navCurrent
                 , field @"stApp" . field @"stTOCShowStage" .~ Set.singleton 1
                 ]
          setRoute $ eStart $> stageUrl @key 1

      elPandoc defaultConfig part3

    pure envNavigation
