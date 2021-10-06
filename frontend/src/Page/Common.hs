{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Page.Common where

import           Common.Alphabet        (PTChar (..), mkPTChord)
import           Common.Route           (FrontendRoute (..))
import           Control.Applicative    (Applicative (pure))
import           Control.Lens           ((%~), (.~), (<&>))
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.Bool              (Bool (..))
import           Data.Eq                (Eq ((==)))
import           Data.Function          (($))
import           Data.Functor           (void, ($>))
import           Data.Generics.Product  (field)
import           Data.Monoid            (Monoid (mconcat))
import           Data.Ord               (Ord ((>)))
import           Data.Semigroup         (Endo (..))
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import           Data.Witherable        (Filterable (filter))
import           Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl,
                                         SetRoute (setRoute), routeLink)
import           Reflex.Dom             (DomBuilder, EventName (Click),
                                         EventWriter, HasDomEvent (domEvent),
                                         MonadHold (holdDyn),
                                         PostBuild (getPostBuild), Prerender,
                                         Reflex (Event, never), blank, el,
                                         elClass, elClass', leftmost, text)
import           Shared                 (dynSimple, iFa, if', whenJust)
import           State                  (EStateUpdate, Env (..),
                                         Navigation (..), Stage (..), stageUrl,
                                         updateState)
import           Text.Show              (Show (show))

elFooter
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , Prerender js t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  )
  => Navigation
  -> m ()
elFooter Navigation{..} = el "footer" $ do
  whenJust navMPrevious $ \prv -> do
    elClass "div" "floatLeft" $ do
      text "< "
      routeLink (stageUrl prv) $ text $ Text.pack $ show prv
  text $ Text.pack $ show navCurrent
  whenJust navMNext $ \nxt -> do
    elClass "div" "floatRight" $ do
      routeLink (stageUrl nxt) $ text $ Text.pack $ show nxt
      text " >"
  elClass "br" "clearBoth" blank

elCongraz
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Env t) m
  , PostBuild t m
  , SetRoute t (R FrontendRoute) m
  )
  => Event t ()
  -> Navigation
  -> m ()
elCongraz eDone Navigation{..} = mdo

  eChord <- asks envEChord

  let chordCON = mkPTChord [LeftC, LeftO, RightN]
      chordBACK = mkPTChord [LeftP, LeftCross, RightA, RightC]
      eChordCON = void $ filter (== chordCON) eChord
      eChordBACK = void $ filter (== chordBACK) eChord

  dynShowCongraz <- holdDyn False $ leftmost [eDone $> True, eBack $> False]
  eBack <- dynSimple $ dynShowCongraz <&> \case
    False -> pure never
    True ->
      elClass "div" "mkOverlay" $
        elClass "div" "congraz" $ do
          el "div" $ text "Task cleared!"
          el "div" $ iFa "fas fa-check-circle"
          whenJust navMNext $ \nxt -> do
            (elACont, _) <- elClass "div" "anthrazit" $ do
              text "Type "
              el "code" $ text "CON"
              text " to continue to "
              elClass' "a" "normalLink" (text $ Text.pack $ show nxt)
            let eContinue = leftmost [eChordCON, domEvent Click elACont]
            updateState $ eContinue $> appEndo (mconcat
              [ Endo $ field @"stProgress" %~ \s -> if nxt > s then nxt else s
              , Endo $ field @"stCleared" %~ Set.insert navCurrent
              , if' (nxt == Stage2_1) $
                  Endo $ field @"stTOCShowStage2" .~ True
              ])
            setRoute $ eContinue $> FrontendRoute_Main :/ ()
          el "div" $ do
            el "span" $ text "("
            (elABack, _) <- elClass' "a" "normalLink" $ text "back"
            text " "
            el "code" $ text "P+AC"
            el "span" $ text ")"
            pure $ leftmost [eChordBACK, domEvent Click elABack]
  blank
