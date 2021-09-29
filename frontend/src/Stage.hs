{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}

module Stage where

import           Common.Alphabet        (PTChar, showKey)
import           Common.Api             (PloverCfg (pcfgMapStenoKeys))
import           Common.Route           (FrontendRoute(..))
import           Control.Applicative    ((<$>))
import           Control.Category       (Category ((.)))
import           Control.Lens           ((<&>))
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.Reader   (Functor (fmap), MonadReader (ask),
                                         MonadTrans (lift))
import           Data.Bool              (Bool (..))
import           Data.Eq                (Eq ((==)))
import           Data.Foldable          (Foldable (length), for_)
import           Data.Function          (($))
import           Data.Int               (Int)
import           Data.List              (sort, zip, (!!))
import qualified Data.Map               as Map
import           Data.Maybe             (Maybe (..))
import           Data.Ord               (Ord ((>)))
import           Data.Semigroup         (Semigroup ((<>)))
import           Data.Set               (Set)
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import           GHC.Num                (Num ((+), (-)))
import           Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, SetRoute(setRoute),  routeLink)
import           Reflex.Dom             (DomBuilder, MonadHold (holdDyn),
                                         PostBuild, Prerender,
                                         Reflex (Dynamic, Event, updated),
                                         blank, dynText, dyn_, el, elClass,
                                         elDynAttr, elDynClass, foldDyn, text,
                                         widgetHold_)
import           State                  (Stage (..), State (stPloverCfg),
                                         stageUrl)
import Shared (iFa)
import Data.Witherable (Filterable(catMaybes))
import Data.Functor (($>))

elFooterNextStage
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , Prerender js t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  )
  => Stage
  -> m ()
elFooterNextStage nxt = do
  el "hr" blank
  el "div" $ do
    text "Advance to "
    routeLink (stageUrl nxt) $ text "Stage 1.2"
    text " by completing the tasks or skip "
    -- TODO: define chords to navigate
    elClass "code" "chord" $ text "ASDF"
    text "."

-- 1.1

data WalkState = WalkState
  { wsCounter  :: Int
  , wsMMistake :: Maybe (Int, Set PTChar)
  , wsDone     :: Bool
  }

stage1_1
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Dynamic t State, Event t (Set PTChar)) m
  , Prerender js t m
  , PostBuild t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  )
  => m ()
stage1_1 = do
  (dynState, eWord) <- ask
  el "h1" $ text "Stage 1"
  el "h2" $ text "The stenographic alphabet"
  el "h3" $ text "Task 1"
  el "span" $ text "Type the following steno letters in order, one after another:"

  let dynAlphabet = Map.keys . pcfgMapStenoKeys . stPloverCfg <$> dynState
  dyn_ $ dynAlphabet <&> \ptAlphabet -> do

    let len = length ptAlphabet
        step :: Set PTChar -> WalkState -> WalkState
        step w ws@WalkState{..} = case (Set.toList w, wsMMistake, wsDone) of
          (_, _, True)         -> ws                          -- done
          _ | wsCounter == len -> ws { wsDone = True }
          (_, Just _, _)       -> ws { wsCounter = 0
                                     , wsMMistake = Nothing
                                     }                        -- reset
          ([l], _, _) | ptAlphabet !! wsCounter == l ->
                                  ws { wsCounter = wsCounter + 1
                                     }                        -- correct
          (ls,  _, _)          -> ws { wsMMistake =
                                         Just ( wsCounter , Set.fromList ls)
                                     }                        -- mistake

        stepInitial = WalkState
          { wsCounter = 0
          , wsMMistake = Nothing
          , wsDone = False
          }
    dynWalk <- foldDyn step stepInitial eWord
    let eDone = catMaybes $ updated $ dynWalk <&> \ws ->
          if wsDone ws then Just () else Nothing

    el "pre" $ el "code" $
      for_ (zip [0 :: Int ..] ptAlphabet) $ \(i, c) -> do
        let dynCls = dynWalk <&> \WalkState{..} -> case wsMMistake of
              Just (j, _) -> if i == j then "bgRed" else ""
              Nothing     -> if wsCounter > i then "bgGreen" else ""
        elDynClass "span" dynCls $ text $ showKey c

    widgetHold_ blank $ updated dynWalk <&> \WalkState{..} -> case wsMMistake of
      Just (_, w)  -> elClass "div" "red small" $ text
        -- TODO: function to properly show steno words
         $ "You typed " <> Text.unwords (showKey <$> Set.toList w)
        <> ". Any key to start over."
      Nothing -> blank

    widgetHold_ blank $ eDone <&> \_ -> elClass "div" "mkOverlay" $ do
      el "div" $ text "Task cleared!"
      el "div" $ iFa "fas fa-check-circle"
      el "div" $ text "Press any key to continue"
      setRoute $ eWord $> FrontendRoute_Stage1_2 :/ ()
      el "div" $ do
        el "span" $ text "("
        routeLink (FrontendRoute_Stage1_1 :/ ()) $ text "back"
        el "span" $ text ")"

  el "span" $ text "Some letters occur twice, the first time for your left hand \
                   \and the second time for your right hand."

  elFooterNextStage Stage1_2
