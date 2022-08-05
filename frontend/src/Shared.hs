{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Shared where

import Data.Generics.Product (field)
import GHC.Real (div, mod, realToFrac, fromIntegral, floor)
import           Control.Applicative            ( Applicative(pure)
                                                , (<$>)
                                                )
import           Control.Category               ( Category((.)) )
import           Control.Lens                   ((.~) )
import           Control.Monad                  ( (=<<) )
import           Data.Function                  (flip,  ($))
import           Data.Functor                   (($>),  void, (<&>) )
import           Data.Maybe                     ( Maybe(..) )
import           Data.Monoid                    (Endo,  (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Tuple                     ( fst )
import           Reflex.Dom                     (leftmost, current, tag, EventWriter, Prerender,  dyn_
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
                                                , inputElementConfig_elementConfig
                                                , inputElementConfig_initialChecked
                                                , switchHold
                                                , text
                                                , (&)
                                                , (=:)
                                                )
import           Data.Bool                      (Bool (..) )
import           Control.Monad.Reader           (asks,  ask
                                                , MonadReader
                                                )
import           State                          (updateState, Session (..),  State (..))
import Data.Int (Int)
import TextShow (TextShow(showt))
import Control.Monad (unless)
import           GHCJS.DOM                      ( currentWindowUnchecked )
import GHCJS.DOM.Location (assign)
import         GHCJS.DOM.Window               (getDocument)
import GHCJS.DOM.Document (getLocationUnchecked)
import Language.Javascript.JSaddle (MonadJSM, liftJSM)
import Control.Monad (Monad((>>=)))
import Data.Time (NominalDiffTime)
import GHC.Float (Double)
import Text.Printf (printf)
import GHC.Num ((*), Num((-)))
import Data.Ord (Ord((>)))
import Obelisk.Route.Frontend (setRoute, SetRoute, R, pattern (:/))
import Common.Route (FrontendRoute_AuthPages (..), showRoute, FrontendRoute (..))
import Data.Either (Either(Right))
import Client ( getMaybeAuthData, postEventViewPage, request )
import Common.Auth (SessionData(..))
import Data.Functor (Functor(fmap))
import Control.Monad (when)

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
        dynMStr = dynStr <&> \s -> if Text.null s then Nothing else Just s
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
        dynMStr = dynStr <&> \s -> if Text.null s then Nothing else Just s
    pure (dynMStr, i)

elButtonSubmit
  :: DomBuilder t m
  => Text
  -> m ()
  -> m (Event t ())
elButtonSubmit cls inner = do
    (e, _) <- elAttr' "button"
                      (  "type" =: "submit"
                      <> "class" =: cls
                      )
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

elFatalError :: DomBuilder t m => Text -> m ()
elFatalError strMessage = elClass "div" "mkOverlay" do
    el "div" $ do
      iFa "fas fa-bomb"
      iFa "fas fa-bomb"
      iFa "fas fa-bomb"
      text " Fatal Error "
      iFa "fas fa-bomb"
      iFa "fas fa-bomb"
      iFa "fas fa-bomb"
    unless (Text.null strMessage) $ el "p" $ text strMessage

loadingScreen :: DomBuilder t m => Text -> m ()
loadingScreen strMessage = elClass "div" "mkOverlay" do
    el "div" $ do
      iFa "fas fa-spinner fa-spin"
      text " Loading"
    unless (Text.null strMessage) $ el "p" $ text strMessage

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

redirectToWikipedia
  :: forall m . MonadJSM m => Text -> m ()
redirectToWikipedia str = liftJSM $
    currentWindowUnchecked >>=
        getDocument >>=
            getLocationUnchecked >>=
                flip assign ("https://en.wikipedia.org/wiki/" <> str)

-- in time 1.8.0.2 there is not FormatTime instance for Difftime
-- (or NominalDifftime)
-- and GHC 8.6.5 depends on that one specifically
formatTime :: NominalDiffTime -> Text
formatTime dt =
    let seconds = realToFrac dt
        secondsFull = floor @Double @Int seconds
        secondsTenth = floor @Double @Int $ (seconds - fromIntegral secondsFull) * 10
        minutes = secondsFull `div` 60
        strMinutes = if minutes > 0 then printf "%2d:" minutes else ""
        strSeconds = printf "%02d." $ secondsFull `mod` 60
    in  Text.pack (strMinutes <> strSeconds) <> showt secondsTenth <> "s"

requestPostViewPage
  :: forall m t
  . ( MonadReader (Dynamic t State) m
    , PostBuild t m
    , Prerender t m
    )
  => Dynamic t (R FrontendRoute)
  -> Event t ()
  -> m ()
requestPostViewPage dynRoute ev = do
  dynState <- ask
  void $ request $ postEventViewPage
    (getMaybeAuthData <$> dynState)
    (Right . showRoute <$> dynRoute)
    ev

elLoginSignup
  :: forall t (m :: * -> *)
  . ( DomBuilder t m,
      PostBuild t m,
      EventWriter t (Endo State) m,
      MonadReader (Dynamic t State) m,
      SetRoute t (R FrontendRoute) m
    )
  => Dynamic t (R FrontendRoute)
  -> m ()
elLoginSignup dynRedirectRoute = elClass "div" "login-signup floatRight" $ do
    dynSession  <- asks $ fmap stSession
    dyn_ $ dynSession <&> \case
        SessionAnon -> do
          (domLogin, _) <- elClass' "a" "normalLink" $ text "Log in"
          let evLogin = domEvent Click domLogin
          setRoute $ evLogin $> FrontendRoute_Auth :/ AuthPage_Login :/ ()
          el "span" $ text " or "
          (domSignup, _) <- elClass' "a" "normalLink" $ text "sign up"
          let evSignup = domEvent Click domSignup
          setRoute $ evSignup $> FrontendRoute_Auth :/ AuthPage_SignUp :/ ()
          updateState $
            tag (current dynRedirectRoute)
                (leftmost [evSignup, evLogin]) <&> \r ->
              [ field @"stRedirectUrl" .~ r ]
        SessionUser SessionData{..} -> do
          el "span" $ text "Logged in as "
          el "span" $ text sdAliasName
          el "span" $ text " ("
          (domLogout, _) <- elClass' "a" "normalLink" $ text "log out"
          el "span" $ text ")"
          when sdIsSiteAdmin $ do
            el "span" $ text " "
            domAdmin <- elClass "span" "icon-link small" $ iFa' "fas fa-lock"
            let evClickAdmin = domEvent Click domAdmin
            setRoute $ evClickAdmin $> FrontendRoute_Admin :/ ()
            updateState $ tag (current dynRedirectRoute) evClickAdmin <&> \r ->
              [ field @"stRedirectUrl" .~ r ]
          let evLogout = domEvent Click domLogout
          updateState $ evLogout $> [ field @"stSession" .~ SessionAnon ]
