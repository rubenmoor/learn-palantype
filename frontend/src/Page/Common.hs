{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Page.Common where

import           Common.Api                     ( Lang(..) )
import           Common.Route                   ( FrontendRoute(..) )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( Category(id) )
import           Control.Lens                   ( (%~)
                                                , (.~)
                                                , (<&>)
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                )
import           Data.Bool                      ( Bool(..) )
import           Data.Either                    ( Either(..) )
import           Data.Eq                        ( Eq((==)) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( ($>)
                                                , void
                                                )
import           Data.Generics.Product          ( field )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( Maybe(..) )
import           Data.Ord                       ( Ord((>)) )
import           Data.Semigroup                 ( Endo(..)
                                                , Semigroup((<>))
                                                )
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import           Data.Witherable                ( Filterable(filter) )
import           Obelisk.Route.Frontend         ( pattern (:/)
                                                , R
                                                , RouteToUrl
                                                , SetRoute(setRoute)
                                                , routeLink
                                                )
import           Palantype.Common               ( Chord
                                                , Palantype
                                                )
import           Palantype.Common.RawSteno      ( RawSteno
                                                , parseChordLenient
                                                , parseSteno
                                                )
import           Reflex.Dom                     ( DomBuilder
                                                , EventName(Click)
                                                , EventWriter
                                                , HasDomEvent(domEvent)
                                                , MonadHold(holdDyn)
                                                , PostBuild(getPostBuild)
                                                , Prerender
                                                , Reflex(Event, never)
                                                , blank
                                                , el
                                                , elClass
                                                , elClass'
                                                , leftmost
                                                , text
                                                )
import           Shared                         ( dynSimple
                                                , iFa
                                                , whenJust
                                                )
import           State                          ( Env(..)
                                                , Message(..)
                                                , Navigation(..)
                                                , Stage(..)
                                                , State
                                                , stageUrl
                                                , updateState
                                                )
import           Text.Show                      ( Show(show) )
import           TextShow                       ( showt )

elFooter
    :: forall js t (m :: * -> *)
     . ( DomBuilder t m
       , Prerender js t m
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       )
    => Lang
    -> Navigation
    -> m ()
elFooter lang Navigation {..} = elClass "footer" "stage" $ do
    whenJust navMPrevious $ \prv -> do
        elClass "div" "floatLeft" $ do
            text "< "
            routeLink (stageUrl lang prv) $ text $ Text.pack $ show prv
    text $ Text.pack $ show navCurrent
    whenJust navMNext $ \nxt -> do
        elClass "div" "floatRight" $ do
            routeLink (stageUrl lang nxt) $ text $ Text.pack $ show nxt
            text " >"
    elClass "br" "clearBoth" blank

getChordCon
    :: forall key t
     . (Palantype key, Reflex t)
    => Lang
    -> Event t (Chord key)
    -> (RawSteno, Event t ())
getChordCon lang eChord =
    let rs = case lang of
            DE -> "GDON"
            EN -> "CON"
    in  (rs, getChord eChord rs)

getChordBack
    :: forall key t
     . (Palantype key, Reflex t)
    => Lang
    -> Event t (Chord key)
    -> (RawSteno, Event t ())
getChordBack lang eChord =
    let rs = case lang of
            DE -> "BÄK"
            EN -> "P+AC"
    in  (rs, getChord eChord rs)

getChord
    :: forall key t
     . (Palantype key, Reflex t)
    => Event t (Chord key)
    -> RawSteno
    -> Event t ()
getChord eChord rs = void $ filter (== parseChordLenient rs) eChord

elCongraz
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , SetRoute t (R FrontendRoute) m
       )
    => Event t ()
    -> Navigation
    -> m ()
elCongraz eDone Navigation {..} = mdo

    eChord <- asks envEChord
    let (rsCon , eChordCon ) = getChordCon navLang eChord
        (rsBack, eChordBack) = getChordBack navLang eChord

    dynShowCongraz <- holdDyn False $ leftmost [eDone $> True, eBack $> False]
    eBack          <- dynSimple $ dynShowCongraz <&> \case
        False -> pure never
        True  -> elClass "div" "mkOverlay" $ elClass "div" "congraz" $ do
            el "div" $ text "Task cleared!"
            el "div" $ iFa "fas fa-check-circle"
            whenJust navMNext $ \nxt -> do
                (elACont, _) <- elClass "div" "anthrazit" $ do
                    text "Type "
                    el "code" $ text $ showt rsCon
                    text " to continue to "
                    elClass' "a" "normalLink" (text $ Text.pack $ show nxt)
                let eContinue = leftmost [eChordCon, domEvent Click elACont]
                updateState
                    $  eContinue
                    $> [ field @"stProgress"
                           %~ Map.update
                                  (\s -> if nxt > s then Just nxt else Just s)
                                  navLang
                       , field @"stCleared" %~ Set.insert navCurrent
                       , if nxt == Stage2_1
                           then field @"stTOCShowStage2" .~ True
                           else id
                       ]
                setRoute $ eContinue $> FrontendRoute_Main :/ ()
            el "div" $ do
                el "span" $ text "("
                (elABack, _) <- elClass' "a" "normalLink" $ text "back"
                text " "
                el "code" $ text $ showt rsBack
                el "span" $ text ")"
                pure $ leftmost [eChordBack, domEvent Click elABack]
    blank

parseStenoOrError
    :: forall proxy key t (m :: * -> *)
     . (EventWriter t (Endo State) m, Palantype key, PostBuild t m)
    => proxy key
    -> RawSteno
    -> m (Maybe [Chord key])
parseStenoOrError _ raw = case parseSteno raw of
    Right words -> pure $ Just words
    Left  err   -> do
        ePb <- getPostBuild
        let msgCaption = "Internal error"
            msgBody =
                "Could not parse steno code: " <> showt raw <> "\n" <> err
        updateState $ ePb $> [field @"stMsg" .~ Just Message { .. }]
        pure Nothing

-- getMapTop2k :: IO (HashMap RawSteno Text, HashMap Text [RawSteno])
-- getMapTop2k = HashMap.empty

elNotImplemented :: forall (m :: * -> *) t . DomBuilder t m => m ()
elNotImplemented = elClass "blockquote" "warning" $ do
    el "strong" $ text "Not implemented"
    el "br" blank
    text
        "You are currently looking at an exercise that has not been \
     \implemented for the original English palantype. \
     \Feel free to read, but don't expect things to work from here on."

rawToggleKeyboard :: Lang -> RawSteno
rawToggleKeyboard = \case
    DE -> "BDJN"
    EN -> "STFL"

rawArrowDown :: Lang -> RawSteno
rawArrowDown = \case
  DE -> "JMKSD"
  EN -> "FCFTS"

rawArrowUp :: Lang -> RawSteno
rawArrowUp = \case
  DE -> "DMKSD"
  EN -> "TCFTS"

backUp :: Lang -> RawSteno
backUp = \case
    EN -> "ULFTS"
    DE -> "ILKSD"
