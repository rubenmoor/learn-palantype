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
                                                , (<&>)
                                                )
import           Data.Int                       ( Int )
import           Data.List                      ( zip )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( Maybe(..) )
import           Data.Semigroup                 ( (<>) )
import qualified Data.Text                     as Text
import           Obelisk.Route.Frontend         ( R
                                                , RouteToUrl
                                                , SetRoute
                                                , routeLink
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
                                                , text
                                                )
import           Servant.API                    ( ToHttpApiData(toUrlPiece) )
import           Shared                         ( iFa
                                                , iFaAttr
                                                )
import           State                          ( Env(..)
                                                , Navigation(..)
                                                , stageUrl
                                                )
import           Text.Show                      ( Show(show) )
import           TextShow                       ( TextShow(showt) )

overview
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
overview = do
    Env {..} <- ask
    let Navigation {..} = envNavigation

    el "h1" $ text "Pattern group overview"

    let iPatternDoc = zip [1 ..] $ Map.toList $ patternDoc @key

    map <- case eMapDictExamples @key of
        Left str -> do
            elClass "span" "red small" $ text $ "Error: " <> str
            pure Map.empty
        Right m -> pure m

    -- TOC embedded in content
    elClass "div" "embeddedToc" $ mdo
        dynToggle <- foldDyn (const not) False (domEvent Click elToggle)
        elToggle  <- el "div" $ do
            el "strong" $ text "Contents"
            text " ("
            (elT, _) <-
                elClass' "a" "normalLink"
                $   dynText
                $   bool "show" "hide"
                <$> dynToggle
            text ")"
            pure elT
        let dynCls = bool "displayNone" "block" <$> dynToggle
        elDynClass "ul" dynCls $ for_
            iPatternDoc
            \(i, (p, _)) -> do
                el "li"
                    $  elAttr "a" ("href" =: (toUrlPiece navLang <> "/" <> Text.pack (show navCurrent) <> "#" <> showt p))
                    $  text
                    $  showt @Int i
                    <> " "
                    <> toDescription p

    -- pattern documentation content
    for_ iPatternDoc \(i, (p, mapPattern)) -> do
        elAttr "h2" ("id" =: showt p)
            $  elAttr "a" ("href" =: (toUrlPiece navLang <> "/" <> Text.pack (show navCurrent) <> "#" <> showt p))
            $  text
            $  showt i
            <> " "
            <> toDescription p
        el "div" $ for_ (Map.toList mapPattern) $ \(g, doc) -> do
            let (n, lsExamples) =
                    Map.findWithDefault (0, []) g
                        $ Map.findWithDefault Map.empty p map
            el "h3" $ do
                text $ "Greediness " <> showt g
                case findStage (StageGeneric p g) of
                    Just (si, _, _) -> routeLink (stageUrl @key si)
                        $ iFaAttr "fas fa-book-open" ("title" =: "Go to exercise")
                    Nothing ->
                        elClass "span" "lightgray" $ iFa "fas fa-book-open"

            elClass "div" "patternExamples" $ mdo
                elClass "strong" "floatLeft" $ text "Examples"
                (btn, _) <-
                    elClass' "button" "floatLeft"
                    $   dyn_
                    $   dynToggle
                    <&> \case
                            False -> iFa "fas fa-plus"
                            True  -> iFa "fas fa-minus"
                elClass "em" "floatRight" $ do
                    text "# total: "
                    text $ showt n
                elClass "br" "clearBoth" blank

                dynToggle <- foldDyn (const not) False $ domEvent Click btn
                let dynShow = bool "whiteSpaceNoWrap" "" <$> dynToggle
                elDynClass "div" dynShow $ for_ lsExamples $ \(w, s) -> do
                    el "span" $ text w
                    text " "
                    el "code" $ text $ showt s
                    text ", "

            elPatterns $ Map.toList doc

    pure envNavigation
