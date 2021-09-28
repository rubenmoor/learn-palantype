{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stage where

import           Data.Function (($))
import           Reflex.Dom    (Prerender, blank, widgetHold_, elDynClass, elDynAttr, elClass, dyn_, foldDyn, PostBuild, MonadHold(holdDyn), dynText, Reflex(updated, Event, Dynamic), DomBuilder, el, text)
import Common.Alphabet (showKey, PTChar)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Reader (MonadTrans(lift), Functor(fmap), MonadReader(ask))
import qualified Data.Text as Text
import Control.Lens ((<&>))
import Control.Category (Category((.)))
import Data.List (zip, (!!), sort)
import Data.Int (Int)
import Data.Eq (Eq((==)))
import GHC.Num (Num((-), (+)))
import Control.Monad.Fix (MonadFix)
import Data.Foldable (for_)
import Data.Ord (Ord((>)))
import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..))
import Text.Show (Show(show))
import State (Stage(..), stageUrl, State(stPloverCfg))
import Control.Applicative ((<$>))
import qualified Data.Map as Map
import Common.Api (PloverCfg(pcfgMapStenoKeys))
import Data.Semigroup (Semigroup((<>)))
import Obelisk.Route.Frontend (SetRoute, R, RouteToUrl, routeLink)
import Common.Route (FrontendRoute)

stage1_1
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , MonadReader ((Dynamic t State), Event t (Set PTChar)) m
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
  dyn_ $ dynAlphabet <&> \ptAlphabet -> el "pre" $ el "code" $ do

    let step :: Set PTChar -> (Int, Maybe (Int, [PTChar])) -> (Int, Maybe (Int, [PTChar]))
        step w (i, mistake) = case (Set.toList w, mistake) of
          (_, Just _)                     -> (0,     Nothing)      -- reset
          ([l], _) | ptAlphabet !! i == l -> (i + 1, Nothing)      -- correct
          (ls, _)                         -> (0,     Just (i, ls)) -- mistake!

    dynWalk <- foldDyn step (0, Nothing) eWord

    for_ (zip [0 :: Int ..] ptAlphabet) $ \(i, c) -> do
      when
      let dynCls = dynWalk <&> \(counter, mistake) -> case mistake of
            Just (j, _) -> if i == j then "bgRed" else ""
            Nothing     -> if counter > i then "bgGreen" else ""
          elDynClass "span" dynCls $ text $ showKey c

    widgetHold_ blank $ updated dynWalk <&> \(_, mistake) -> case mistake of
      Just (_, w)  -> elClass "div" "red small" $ text
         $ "You typed " <> Text.unwords (showKey <$> w)
        <> ". Any key to start over"
      Nothing -> blank

  el "span" $ text "Some letters occur twice, the first time for your left hand \
                   \and the second time for your right hand."

  el "hr" blank
  el "div" $ do
    text "Advance to "
    routeLink (stageUrl Stage1_1) $ text "Stage 1.1"
    text " by completing the tasks."
  -- TODO: function to properly show steno words
