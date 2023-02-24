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

module Page.Introduction where

import           Client                         ( getCMS
                                                , request
                                                )
import           Common.Model                   ( TextLang(TextEN) )
import           Common.Route                   ( FrontendRoute(..) )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( (.) )
import           Control.Lens                   ( (%~)
                                                , (.~)
                                                )
import           Control.Monad.Reader           ( MonadReader(ask) )
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
import           Data.Monoid                    ( Monoid(mempty) )
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
                                                , Prerender
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
                                                , widgetHold_
                                                )
import           Reflex.Dom.Pandoc              ( defaultConfig
                                                , elPandoc
                                                )
import           Servant.Common.Req             ( QParam(QParamSome) )
import           Shared                         ( loadingScreen )
import           State                          ( Env(..)
                                                , Navigation(..)
                                                , State
                                                , stageUrl
                                                , updateState
                                                )
import           TextShow                       ( TextShow(showt) )
import           Witherable                     ( Filterable(filter) )

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

        systemLang = QParamSome SystemDE
        txtLang    = QParamSome TextEN
        pageName   = QParamSome "introduction"

    (evRespFail, evRespSucc) <- fmap fanEither $ request $
      getCMS (constDyn systemLang)
             (constDyn txtLang   )
             (constDyn pageName  )
             evReady

    widgetHold_ (loadingScreen "") $ evRespFail <&> \strError ->
      el "span" $ text $ "Error loading content: " <> strError

    dynParts <- widgetHold (loadingScreen "" $> (mempty, mempty, mempty)) $ evRespSucc <&> \case
        [p1, p2, p3] -> pure (p1, p2, p3)
        parts        -> do
          el "span" $ text $
                 "wrong number of parts in markdown. Expected: 3. Got: "
              <> showt (length parts)
          pure (mempty, mempty, mempty)

    dyn_ $ dynParts <&> \(part1, part2, part3) -> do

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
