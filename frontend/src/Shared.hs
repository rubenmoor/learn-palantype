{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

module Shared where

import           Control.Applicative (Applicative (pure), (<$>))
import           Control.Category    (Category ((.)))
import           Control.Monad.Fix   (MonadFix)
import           Data.Bool           (Bool, not)
import           Data.Function       (const, ($))
import           Data.Functor        (void)
import           Data.Maybe          (fromMaybe, Maybe (..))
import           Data.Monoid         ((<>))
import           Data.Text           (Text, unwords)
import qualified Data.Text           as Text
import           Data.Tuple          (fst)
import           Reflex.Dom          (PostBuild, NotReady, Adjustable, dyn, switchHold, DomBuilder (DomBuilderSpace, inputElement),
                                      Element, EventName (Click), EventResult,
                                      HasDomEvent (domEvent),
                                      InputElement (_inputElement_checked, _inputElement_value),
                                      InputElementConfig, MonadHold (holdDyn),
                                      Reflex(never, Dynamic, Event, current, updated),
                                      XhrResponse (..), attachWith, blank, def,
                                      el, el', elAttr, elAttr', elClass',
                                      elementConfig_initialAttributes, ffor,
                                      inputElementConfig_elementConfig,
                                      inputElementConfig_initialChecked,
                                      inputElementConfig_setChecked, leftmost,
                                      text, (&), (.~), (=:), switchDyn, Prerender (prerender, Client))
import           Servant.Common.Req  (ReqResult (..))
import Control.Monad (Monad, (=<<))

iFa' :: DomBuilder t m => Text -> m (Element EventResult (DomBuilderSpace m) t)
iFa' class' = fst <$> elClass' "i" class' blank

iFa :: DomBuilder t m => Text -> m ()
iFa = void . iFa'

elLabelInput
  :: DomBuilder t m
  => InputElementConfig e t (DomBuilderSpace m)
  -> Text
  -> Text
  -> m (Dynamic t (Maybe Text), InputElement e (DomBuilderSpace m) t)
elLabelInput conf label id = do
  elAttr "label" ("for" =: id) $ el "h3" $ text label
  i <- inputElement $ conf
         & inputElementConfig_elementConfig
         . elementConfig_initialAttributes
         .~ ("id" =: id <> "type" =: "text")
  let dynStr = _inputElement_value i
      dynMStr = ffor dynStr $ \s -> if Text.null s then Nothing else Just s
  pure (dynMStr, i)

elLabelPasswordInput
  :: DomBuilder t m
  => InputElementConfig e t (DomBuilderSpace m)
  -> Text
  -> Text
  -> m (Dynamic t (Maybe Text), InputElement e (DomBuilderSpace m) t)
elLabelPasswordInput conf label id = do
  elAttr "label" ("for" =: id) $ el "h3" $ text label
  i <- inputElement $ conf
         & inputElementConfig_elementConfig
         . elementConfig_initialAttributes
         .~ ("id" =: id <> "type" =: "password")
  let dynStr = _inputElement_value i
      dynMStr = ffor dynStr $ \s -> if Text.null s then Nothing else Just s
  pure (dynMStr, i)

btnSend
  :: DomBuilder t m
  => m ()
  -> m (Event t ())
btnSend inner = do
  let cls = "class" =: unwords
        [ "onDesktopMaxWidth370px"
        , "onMobileFontBig"
        , "btnSend"
        ]
  (e, _) <- elAttr' "button" cls inner
  pure $ domEvent Click e

checkbox ::
  ( DomBuilder t m
  , MonadHold t m
  , MonadFix m
  ) => Bool -> Text -> m (Dynamic t Bool)
checkbox initial description = mdo
    cb <- inputElement $
      def & inputElementConfig_elementConfig
          . elementConfig_initialAttributes .~ "type" =: "checkbox"
          & inputElementConfig_initialChecked .~ initial
          & inputElementConfig_setChecked .~ eClickCB
    (elSpan, _) <- el' "span" $ text description
    let dynCbChecked = _inputElement_checked cb
        eToggle = leftmost [void $ updated dynCbChecked, domEvent Click elSpan]
        eClickCB = attachWith (const . not) (current dynCbChecked) eToggle
    holdDyn initial eClickCB

reqFailure :: ReqResult tag a -> Maybe Text
reqFailure = \case
  ResponseSuccess {}        -> Nothing
  ResponseFailure _ str xhr -> Just $ str <> fromMaybe "" (_xhrResponse_responseText xhr)
  RequestFailure  _ str     -> Just str

whenJust ::
  forall a t.
  Applicative t =>
  Maybe a ->
  (a -> t ()) ->
  t ()
whenJust (Just x) a = a x
whenJust Nothing _  = pure ()

dynSimple
  :: forall a t (m :: * -> *).
  ( Adjustable t m
  , MonadHold t m
  , NotReady t m
  , PostBuild t m
  )
  => Dynamic t (m (Event t a))
  -> m (Event t a)
dynSimple a = switchHold never =<< dyn a

prerenderSimple
  :: forall a js t (m :: * -> *).
  ( Prerender js t m
  , Applicative m
  )
  => Client m (Event t a)
  -> m (Event t a)
prerenderSimple a = switchDyn <$> prerender (pure never) a
