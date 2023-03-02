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
                                                , request
                                                )
import           Common.Model                   ( TextLang(TextEN) )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( (<<<) )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Reader           ( MonadReader(ask) )
import           Data.Either                    ( Either(..) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( ($>)
                                                , (<$>)
                                                , (<&>)
                                                , Functor(fmap, (<$))
                                                , void
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( Maybe(..)
                                                , fromMaybe
                                                , isNothing
                                                )
import           Data.Ord                       ( Ord((>)) )
import           Data.Semigroup                 ( Semigroup((<>)) )
import           Data.Time                      ( getCurrentTime )
import           Language.Javascript.JSaddle    ( liftJSM )
import qualified LocalStorage                  as LS
import           Reflex.Dom                     ( DomBuilder
                                                , MonadHold (holdDyn)
                                                , PerformEvent(..)
                                                , PostBuild
                                                , Prerender
                                                , Reflex(..)
                                                , blank
                                                , constDyn
                                                , current
                                                , el
                                                , fanEither
                                                , getPostBuild
                                                , leftmost
                                                , prerender_
                                                , tag
                                                , text
                                                , widgetHold
                                                , widgetHold_, dyn_
                                                )
import           Shared                         ( loadingScreen )
import           State                          ( Env(..)
                                                , Navigation(..)
                                                , State
                                                    ( stCMSCacheInvalidationData
                                                    )
                                                )
import           Text.Pandoc.Definition         ( Pandoc )
import           Witherable                     ( Filterable(filter)
                                                , catMaybes
                                                )
import Data.Bool (Bool(..))
import Control.Monad (when)

elCMS
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Prerender t m
       , PostBuild t m
       )
    => (Event t () -> Event t ())
    -> m (Dynamic t [Pandoc])
elCMS toReady = do
    Env {..} <- ask
    evReady <- toReady <$> getPostBuild
    let
        Navigation {..} = envNavigation
        behaviorCacheInvalidation = current $ stCMSCacheInvalidationData <$> envDynState
        cacheKey = (navSystemLang, navTextLang, navPageName)

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
      getCMS (constDyn $ Right navSystemLang       )
             (constDyn $ Right navTextLang        )
             (constDyn $ Right navPageName)
             evNotFromCache

    widgetHold_ blank $ evFromCache $> el "div" (text "Retrieved contents from cache")
    widgetHold_ blank $ evRespSucc $> el "div" (text "Retrieved contents from webserver")

    widgetHold_ blank $ evRespFail <&> \strError ->
      el "span" $ text $ "CMS error: " <> strError

    dynLoading <- holdDyn True $ leftmost
      [ evRespSucc  $> False
      , evFromCache $> False
      , evRespFail  $> False
      ]
    dyn_ $ dynLoading <&> \bLoading -> when bLoading $ loadingScreen ""

    widgetHold (pure []) $
      leftmost [evRespSucc, evFromCache] <&> \parts -> do
        prerender_ blank $ do
          now <- liftIO getCurrentTime
          liftJSM $ LS.update LS.KeyCMSCache $
            Map.insert (navSystemLang, TextEN, "introduction") (now, parts) <<< fromMaybe Map.empty
        pure parts
