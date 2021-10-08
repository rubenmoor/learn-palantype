{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Shared where

import           Client                (RequestResult (..), postLookupSteno,
                                        request)
import           Common.Alphabet       (PTChord (PTChord))
import           Control.Applicative   (Applicative (pure), (<$>))
import           Control.Category      (Category ((.)))
import           Control.Lens          ((.~))
import           Control.Monad         ((=<<))
import           Control.Monad.Fix     (MonadFix)
import           Data.Bool             (Bool (..), not)
import           Data.Either           (Either (..))
import           Data.Function         (const, ($))
import           Data.Functor          (void, ($>), (<&>))
import           Data.Generics.Product (field)
import qualified Data.Map              as Map
import           Data.Maybe            (Maybe (..))
import           Data.Monoid           (Monoid (mempty), (<>))
import           Data.Semigroup        (Endo (Endo))
import           Data.Text             (Text, unwords)
import qualified Data.Text             as Text
import           Data.Traversable      (for)
import           Data.Tuple            (fst)
import           Reflex.Dom            (Adjustable, DomBuilder (DomBuilderSpace, inputElement),
                                        Element, EventName (Click), EventResult,
                                        EventWriter, HasDomEvent (domEvent),
                                        InputElement (_inputElement_checked, _inputElement_value),
                                        InputElementConfig, MonadHold (holdDyn),
                                        NotReady, PostBuild (getPostBuild),
                                        Prerender (Client, prerender),
                                        Reflex (Dynamic, Event, current, never, updated),
                                        attachWith, blank, constDyn, def, dyn,
                                        el, el', elAttr, elAttr', elClass,
                                        elClass',
                                        elementConfig_initialAttributes, ffor,
                                        inputElementConfig_elementConfig,
                                        inputElementConfig_initialChecked,
                                        inputElementConfig_setChecked, leftmost,
                                        switchDyn, switchHold, text, widgetHold,
                                        (&), (.~), (=:))
import           State                 (Message (..), State (..), updateState)
import qualified StenoExpressions

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

widgetHoldSimple
  :: forall a t (m :: * -> *).
  ( Adjustable t m
  , MonadHold t m
  )
  => Event t (m (Event t a)) -> m (Event t a)
widgetHoldSimple a = switchDyn <$> widgetHold (pure never) a

loadingScreen
  :: DomBuilder t m
  => m ()
loadingScreen =
  elClass "div" "mkOverlay" $ do
    iFa "fas fa-spinner fa-spin"
    text " Loading ..."

if' :: Monoid a => Bool -> a -> a
if' True x  = x
if' False _ = mempty

lookupSteno
  :: forall js t (m :: * -> *).
  ( EventWriter t (Endo State) m
  , PostBuild t m
  , Prerender js t m
  )
  => Text
  -> m (Event t [PTChord])
lookupSteno str = do
  ePb <- getPostBuild
  let words = Text.words str
      mChords = for words $ \w ->
        Map.lookup w StenoExpressions.dict
  case mChords of
    Just chords -> pure $ ePb $> chords
    Nothing     -> do
      RequestResult{..} <- request $ postLookupSteno (constDyn $ Right words) ePb

      updateState $ rrEFailure <&> \err ->
        let msgCaption = "Internal error"
            msgBody = "Could not parse steno code: \n"
                   <> str <> "\n" <> err
        in  [field @"stMsg" .~ Just Message{..}]

      pure rrESuccess
