{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Page.Patterns where

--
import           Common.Route                   ( FrontendRoute )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.Reader.Class     ( MonadReader
                                                , ask
                                                )
import           Data.Bool                      ( Bool(..)
                                                , bool
                                                , not
                                                )
import           Data.Either                    ( Either(..) )
import           Data.Foldable                  ( for_ )
import           Data.Function                  ( ($)
                                                , const
                                                )
import           Data.Functor                   ( (<$>)
                                                , (<&>), ($>)
                                                )
import           Data.Int                       ( Int )
import           Data.List                      ( zip )
import qualified Data.Map.Strict               as Map
import           Data.Semigroup                 ( (<>), Endo )
import qualified Data.Text                     as Text
import           Obelisk.Route.Frontend         ( R
                                                , RouteToUrl
                                                , SetRoute

                                                )
import           Page.Common                    ( elPatterns )
import           Palantype.Common               ( Palantype
                                                , StageSpecialGeneric
                                                    ( StageGeneric
                                                    )
                                                , findStage
                                                , patternDoc
                                                , toDescription
                                                )
import           PloverDict                     ( eMapDictExamples )
import           Reflex.Dom                     ( (=:)
                                                , DomBuilder
                                                , EventName(Click)
                                                , MonadHold
                                                , PostBuild
                                                , Prerender
                                                , blank
                                                , domEvent
                                                , dynText
                                                , dyn_
                                                , el
                                                , elAttr
                                                , elClass
                                                , elClass'
                                                , elDynClass
                                                , foldDyn
                                                , text, el', EventWriter, PerformEvent (..), TriggerEvent
                                                )
import           Servant.API                    ( ToHttpApiData(toUrlPiece) )
import           Shared                         ( iFa
                                                , iFaAttr, elRouteLink
                                                )
import           State                          ( Env(..)
                                                , Navigation(..)
                                                , stageUrl, State, Loading (LoadingDone), updateState
                                                )
import           Text.Show                      ( Show(show) )
import           TextShow                       ( TextShow(showt) )
import Palantype.Common.TH (fromJust)
import Data.Generics.Product (HasField(..))
import Control.Lens ((.~))
import Control.Monad.IO.Class (MonadIO)

overview
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadIO (Performable m)
       , MonadReader (Env t key) m
       , Palantype key
       , PerformEvent t m
       , PostBuild t m
       , Prerender t m
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       , TriggerEvent t m
       )
    => m ()
overview = do
    Env {..} <- ask
    let Navigation {..} = envNavigation

    evLoadedAndBuilt <- envGetLoadedAndBuilt
    updateState $ evLoadedAndBuilt $> [ field @"stLoading" .~ LoadingDone ]

    elClass "h1" "text-4xl font-bold py-6" $ text "Pattern group overview"

    let iPatternDoc = zip [1 ..] $ Map.toList $ patternDoc @key

    map <- case eMapDictExamples @key of
        Left str -> do
            elClass "span" "text-red-500 text-xs" $ text $ "Error: " <> str
            pure Map.empty
        Right m -> pure m

    -- TOC embedded in content
    elClass "div" "border border-grayishblue-900 rounded text-sm p-2" mdo
        dynToggle <- foldDyn (const not) False (domEvent Click elToggle)
        elToggle  <- el "div" $ do
            el "strong" $ text "Contents"
            text " ("
            (elT, _) <- el' "a" $ dynText $ bool "show" "hide" <$> dynToggle
            text ")"
            el "br" blank
            pure elT
        let dynCls = bool "hidden" "" <$> dynToggle
        elDynClass "ul" dynCls $ for_ iPatternDoc \(i, (p, _)) ->
          el "li"
              $  elAttr "a" ("href" =: ("DE/" <> Text.pack (show navCurrent) <> "#" <> showt p))
              $  text $  showt @Int i <> " " <> toDescription p

    -- pattern documentation content
    for_ iPatternDoc \(i, (p, mapPattern)) -> do
        elAttr "h2" ( "id" =: showt p
                   <> "class" =: "text-3xl font-bold text-grayishblue-900 py-4"
                    )
            $  elAttr "a"
                ("href" =:
                  ( toUrlPiece navSystemLang
                 <> "/" <> Text.pack (show navCurrent)
                 <> "#" <> showt p
                  )
                )
            $  text $  showt i <> " " <> toDescription p
        el "div" $ for_ (Map.toList mapPattern) $ \(g, doc) -> do
            let (n, lsExamples) =
                    Map.findWithDefault (0, []) g
                        $ Map.findWithDefault Map.empty p map
            elClass "h3" "text-2xl text-grayishblue-800 pt-4 pb-3" do
                text $ "Greediness " <> showt g
                let (si, _, _) = $fromJust $ findStage (StageGeneric p g)
                elClass "span" "ml-1 text-zinc-500"
                  $ elRouteLink (stageUrl @key si)
                  $ iFaAttr "fas fa-book-open" ("title" =: "Go to exercise")

            elClass "div" "bg-grayishblue-200 text-lg p-2 rounded" mdo
                elClass "strong" "float-left" $ text "Examples "
                (btn, _) <-
                    elClass' "button" "float-left"
                      $ dyn_ $ dynToggle <&> \case
                          False -> iFa "fas fa-plus"
                          True  -> iFa "fas fa-minus"
                elClass "em" "float-right" do
                    text "# total: "
                    text $ showt n
                elClass "br" "clear-both" blank

                dynToggle <- foldDyn (const not) False $ domEvent Click btn
                let dynShow = bool "whitespace-nowrap overflow-hidden" "" <$> dynToggle
                elDynClass "div" dynShow $ for_ lsExamples $ \(w, s) -> do
                    el "span" $ text w
                    text " "
                    el "code" $ text $ showt s
                    text ", "

            elPatterns $ Map.toList doc
