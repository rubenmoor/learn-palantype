{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shared where

import           Control.Applicative            ( Applicative(pure)
                                                , (<$>)
                                                )
import           Control.Category               ( Category((.)) )
import           Control.Lens                   ( (.~) )
import           Control.Monad                  ( (=<<) )
import           Data.Function                  ( ($) )
import           Data.Functor                   ( void )
import           Data.Maybe                     ( Maybe(..) )
import           Data.Monoid                    ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Tuple                     ( fst )
import           Reflex.Dom                     ( dyn_
                                                , MonadHold
                                                , Adjustable
                                                , DomBuilder
                                                    ( DomBuilderSpace
                                                    , inputElement
                                                    )
                                                , Element
                                                , EventName(Click)
                                                , EventResult
                                                , HasDomEvent(domEvent)
                                                , InputElement
                                                    ( _inputElement_checked
                                                    , _inputElement_value
                                                    )
                                                , InputElementConfig
                                                , NotReady
                                                , PostBuild
                                                , Reflex(Dynamic, Event, never)
                                                , blank
                                                , def
                                                , dyn
                                                , el
                                                , elAttr
                                                , elAttr'
                                                , elClass
                                                , elClass'
                                                , elementConfig_initialAttributes
                                                , ffor
                                                , inputElementConfig_elementConfig
                                                , inputElementConfig_initialChecked
                                                , switchHold
                                                , text
                                                , (&)
                                                , (=:)
                                                )
import           Data.Bool                      ( Bool )
import           Control.Monad.Reader           ( ask
                                                , MonadReader
                                                )
import           State                          ( State )
import           Data.Functor                   ( (<&>) )
import Data.Int (Int)
import TextShow (TextShow(showt))

iFa' :: DomBuilder t m => Text -> m (Element EventResult (DomBuilderSpace m) t)
iFa' class' = fst <$> elClass' "i" class' blank

iFa :: DomBuilder t m => Text -> m ()
iFa = void . iFa'

elLabelInput
    :: DomBuilder t m
    => InputElementConfig e t (DomBuilderSpace m)
    -> Text
    -> Int
    -> Text
    -> m
           ( Dynamic t (Maybe Text)
           , InputElement e (DomBuilderSpace m) t
           )
elLabelInput conf label maxlength id = do
    elAttr "label" ("for" =: id) $ el "h3" $ text label
    i <-
        inputElement
        $  conf
        &  inputElementConfig_elementConfig
        .  elementConfig_initialAttributes
        .~ ("id" =: id <> "type" =: "text" <> "maxlength" =: showt maxlength)
    let dynStr  = _inputElement_value i
        dynMStr = ffor dynStr $ \s -> if Text.null s then Nothing else Just s
    pure (dynMStr, i)

elLabelPasswordInput
    :: DomBuilder t m
    => InputElementConfig e t (DomBuilderSpace m)
    -> Text
    -> Text
    -> m
           ( Dynamic t (Maybe Text)
           , InputElement e (DomBuilderSpace m) t
           )
elLabelPasswordInput conf label id = do
    elAttr "label" ("for" =: id) $ el "h3" $ text label
    i <-
        inputElement
        $  conf
        &  inputElementConfig_elementConfig
        .  elementConfig_initialAttributes
        .~ ("id" =: id <> "type" =: "password" <> "maxlength" =: "64")
    let dynStr  = _inputElement_value i
        dynMStr = ffor dynStr $ \s -> if Text.null s then Nothing else Just s
    pure (dynMStr, i)

btnSubmit :: DomBuilder t m => m () -> m (Event t ())
btnSubmit inner = do
    (e, _) <- elAttr' "button"
                      ("class" =: "button-submit" <> "type" =: "submit")
                      inner
    pure $ domEvent Click e

whenJust :: forall a t . Applicative t => Maybe a -> (a -> t ()) -> t ()
whenJust (Just x) a = a x
whenJust Nothing  _ = pure ()

dynSimple
    :: forall a t (m :: * -> *)
     . (Adjustable t m, MonadHold t m, NotReady t m, PostBuild t m)
    => Dynamic t (m (Event t a))
    -> m (Event t a)
dynSimple a = switchHold never =<< dyn a

-- prerenderSimple
--   :: forall a js t (m :: * -> *).
--   ( Prerender js t m
--   , Applicative m
--   )
--   => Client m (Event t a)
--   -> m (Event t a)
-- prerenderSimple a = switchDyn <$> prerender (pure never) a

loadingScreen :: DomBuilder t m => m ()
loadingScreen = elClass "div" "mkOverlay" $ do
    iFa "fas fa-spinner fa-spin"
    text " Loading ..."

elLabelCheckbox
    :: (DomBuilder t m) => Bool -> Text -> Text -> m (Dynamic t Bool)
elLabelCheckbox initial label id = do
    cb <-
        inputElement
        $  def
        &  inputElementConfig_elementConfig
        .  elementConfig_initialAttributes
        .~ ("type" =: "checkbox" <> "id" =: id)
        &  inputElementConfig_initialChecked
        .~ initial
    elAttr "label" ("for" =: id) $ el "span" $ text label
    pure $ _inputElement_checked cb

undynState
    :: forall t (m :: * -> *)
     . ( MonadReader (Dynamic t State) m
       , Adjustable t m
       , NotReady t m
       , PostBuild t m
       )
    => (State -> m ())
    -> m ()
undynState func = do
    dynState <- ask
    dyn_ $ dynState <&> \st -> func st
