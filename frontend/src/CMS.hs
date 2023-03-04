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
import           Control.Category               ( (<<<), (.) )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Reader           ( MonadReader(ask) )
import           Data.Either                    ( Either(..) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( (<$>)
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
                                                , MonadHold
                                                , PerformEvent(..)
                                                , PostBuild (..)
                                                , Prerender
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
                                                , widgetHold_, TriggerEvent, dyn_, dyn
                                                )
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
import Data.Foldable (Foldable(length))
import Data.Eq (Eq((==)))
import TextShow (TextShow(showt))
import Data.Int (Int)

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
          (time, contents) <- Map.lookup cacheKey map
          if Just time > mLatest
            then pure contents
            else Nothing

    let evNotFromCache = void $ filter isNothing evMFromCache
        evFromCache = catMaybes evMFromCache

    (evRespFail, evRespSucc) <- fmap fanEither $ request $
      getCMS (constDyn $ Right navSystemLang       )
             (constDyn $ Right navTextLang        )
             (constDyn $ Right navPageName)
             evNotFromCache

    widgetHold_ blank $ evRespFail <&> \strError ->
      el "span" $ text $ "CMS error: " <> strError

    dynMParts <- widgetHold (pure Nothing) $
      leftmost [Just <$> evRespSucc, Just <$> evFromCache, Nothing <$ evRespFail] <&> \case
        Just parts -> do
          prerender_ blank $ do
            now <- liftIO getCurrentTime
            liftJSM $ LS.update LS.KeyCMSCache $
              Map.insert (navSystemLang, TextEN, navPageName) (now, parts) <<< fromMaybe Map.empty
          pure $ Just parts
        Nothing -> pure $ Just []

    fmap catMaybes $ dyn $ dynMParts <&> el "div" . \case
        Nothing                               -> Nothing <$ text "Loading content ..."
        Just parts | length parts == numParts -> pure $ Just parts
        Just parts                            -> do
          text ("Wrong number of parts, expected 3, got " <> showt (length parts))
          pure Nothing
