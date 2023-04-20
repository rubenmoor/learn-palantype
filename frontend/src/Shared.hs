{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Shared where

import           Client                         ( getMaybeAuthData
                                                , postEventViewPage
                                                , request
                                                )
import           Common.Auth                    ( SessionData(..) )
import           Common.Route                   ( FrontendRoute(..)
                                                , FrontendRoute_AuthPages(..)
                                                , showRoute, FrontendRoute_AdminPages (AdminPage_Journal)
                                                )
import           Control.Applicative            ( (<$>)
                                                , Applicative(pure)
                                                )
import           Control.Category               ( Category((.)) )
import           Control.Lens                   ( (.~), (%~) )
import           Control.Monad                  ( (=<<)
                                                , Monad((>>=))
                                                , unless
                                                , when
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , ask
                                                , asks
                                                )
import           Data.Bool                      ( Bool(..) )
import           Data.Either                    ( Either(Right) )
import           Data.Function                  ( ($)
                                                , flip, const
                                                )
import           Data.Functor                   ( ($>)
                                                , (<&>)
                                                , Functor(fmap, (<$))
                                                , void
                                                )
import           Data.Generics.Product          ( field )
import           Data.Int                       ( Int )
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( Maybe(..) )
import           Data.Monoid                    ( (<>)
                                                , Endo
                                                )
import           Data.Ord                       ( Ord((>)) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Time                      ( NominalDiffTime )
import           Data.Tuple                     ( fst )
import           GHC.Float                      ( Double )
import           GHC.Num                        ( (*)
                                                , Num((-))
                                                )
import           GHC.Real                       ( div
                                                , floor
                                                , fromIntegral
                                                , mod
                                                , realToFrac
                                                )
import           GHCJS.DOM                      ( currentWindowUnchecked, currentWindow )
import           GHCJS.DOM.Document             ( getLocationUnchecked )
import           GHCJS.DOM.Location             ( assign )
import           GHCJS.DOM.Window               ( getDocument, scrollTo )
import           Language.Javascript.JSaddle    ( MonadJSM
                                                , liftJSM
                                                )
import           Obelisk.Route.Frontend         ( pattern (:/)
                                                , R
                                                , SetRoute
                                                , setRoute, RouteToUrl (..)
                                                )
import           Reflex.Dom                     ( (&)
                                                , (=:)
                                                , Adjustable
                                                , DomBuilder
                                                    (..
                                                    )
                                                , Element
                                                , EventName(Click)
                                                , EventResult
                                                , EventWriter
                                                , HasDomEvent(domEvent)
                                                , InputElement ( _inputElement_checked)

                                                , MonadHold
                                                , NotReady
                                                , PostBuild
                                                , Prerender
                                                , Reflex(Dynamic, Event, never)
                                                , blank
                                                , current
                                                , def
                                                , dyn
                                                , dyn_
                                                , el
                                                , elAttr

                                                , elClass
                                                , elClass'
                                                , elementConfig_initialAttributes
                                                , inputElementConfig_elementConfig
                                                , inputElementConfig_initialChecked
                                                , leftmost
                                                , switchHold
                                                , tag
                                                , text, el', ElementConfig, elementConfig_eventSpec, DomSpace (..), preventDefault, PerformEvent (..), prerender_, holdUniqDyn
                                                )
import           State                          ( Session(..)
                                                , State(..)
                                                , updateState, Loading (..)
                                                )
import           Text.Printf                    ( printf )
import           TextShow                       ( TextShow(showt) )
import Data.Proxy (Proxy (..))
import Control.Monad.Fix (MonadFix)

iFa' :: DomBuilder t m => Text -> m (Element EventResult (DomBuilderSpace m) t)
iFa' class' = fst <$> elClass' "i" class' blank

iFa :: DomBuilder t m => Text -> m ()
iFa = void . iFa'

iFaAttr :: DomBuilder t m => Text -> Map Text Text -> m ()
iFaAttr class' attrs = void $ elAttr "i" ("class" =: class' <> attrs) blank

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

elLoading
  :: forall t (m :: * -> *)
  . ( DomBuilder t m
    , PostBuild t m
    ) => Dynamic t Loading -> m ()
elLoading d = dyn_ $ d <&> elClass "div" "fixed left-1/2 text-zinc-500 top-3" . \case
      LoadingStill msg      -> elAttr "span" ("title" =: msg) $ iFa "fas fa-spinner fa-spin"
      LoadingDone           -> blank
      LoadingError strError -> elAttr "span" ("title" =: strError) $ iFa "far fa-meh"

elLabelCheckbox
    :: (DomBuilder t m) => Bool -> Text -> Text -> m (Dynamic t Bool)
elLabelCheckbox initial label id = do
    cb <-
        inputElement
        $  def
        &  inputElementConfig_elementConfig
        .  elementConfig_initialAttributes
        .~ "type" =: "checkbox" <> "id" =: id
        &  inputElementConfig_initialChecked
        .~ initial
    elAttr "label" ("for" =: id) $ el "span" $ text label
    pure $ _inputElement_checked cb

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
    , MonadHold t m
    , MonadFix m
    , PostBuild t m
    , Prerender t m
    )
  => Dynamic t (R FrontendRoute)
  -> Event t ()
  -> m ()
requestPostViewPage dynRoute ev = do
  dynMAuthData <- ask >>= holdUniqDyn . fmap getMaybeAuthData
  void $ request $ postEventViewPage
    dynMAuthData
    (Right . showRoute <$> dynRoute)
    ev

elLoginSignup
  :: forall t (m :: * -> *)
  . ( DomBuilder t m
    , EventWriter t (Endo State) m
    , MonadHold t m
    , MonadReader (Dynamic t State) m
    , PostBuild t m
    , SetRoute t (R FrontendRoute) m, MonadFix m
    )
  => Dynamic t (R FrontendRoute)
  -> m ()
elLoginSignup dynRedirectRoute =
  elClass "div" "float-right text-lg px-2 pt-1" do
    dynSession  <- holdUniqDyn =<< asks (fmap stSession)
    dyn_ $ dynSession <&> \case
        SessionAnon -> do
          (domLogin, _) <- el' "a" $ text "Log in"
          let evLogin = domEvent Click domLogin
          setRouteAndLoading $ evLogin $> FrontendRoute_Auth :/ AuthPage_Login :/ ()
          el "span" $ text " or "
          (domSignup, _) <- el' "a" $ text "sign up"
          let evSignup = domEvent Click domSignup
          setRouteAndLoading $ evSignup $> FrontendRoute_Auth :/ AuthPage_SignUp :/ ()
          updateState $ tag (current dynRedirectRoute) (leftmost [evSignup, evLogin]) <&>
              \r -> [ field @"stRedirectUrl" .~ r ]
        SessionUser SessionData{..} -> do
          el "span" $ text "Logged in as "
          elClass "span" "font-bold" $ text sdAliasName
          el "span" $ text " ("
          (domLogout, _) <- el' "a" $ text "log out"
          el "span" $ text ")"
          when sdIsSiteAdmin do
            el "span" $ text " "
            domAdmin <- elClass "span" "text-zinc-500 cursor-pointer text-sm"
              $ iFa' "fas fa-lock"
            let evClickAdmin = domEvent Click domAdmin
            setRouteAndLoading $ evClickAdmin $> FrontendRoute_Admin :/ AdminPage_Journal :/ ()
            updateState $ tag (current dynRedirectRoute) evClickAdmin <&> \r ->
              [ field @"stRedirectUrl" .~ r ]
          let evLogout = domEvent Click domLogout
          updateState $ evLogout $> [ field @"stSession" .~ SessionAnon ]

setRouteAndLoading
  :: forall r t (m :: * -> *)
  . ( EventWriter t (Endo State) m
    , SetRoute t r m
    )
  => Event t r
  -> m ()
setRouteAndLoading e = do
  updateState $ e $> [ field @"stLoading" .~ LoadingStill "Building page" ]
  setRoute e

elRouteLink
  :: forall t m a
   . ( DomBuilder t m
     , EventWriter t (Endo State) m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , Prerender t m
     )
    => R FrontendRoute -- ^ Target route
    -> m a             -- ^ Child widget
    -> m a
elRouteLink r w = do
  enc <- askRouteToUrl
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec
            %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m))
                                 Click
                                 (const preventDefault)
        & elementConfig_initialAttributes .~ "href" =: enc r
  (e, a) <- element "a" cfg w
  let evClick = domEvent Click e
  setRouteAndLoading $ r <$ evClick

  -- scrollToTop e
  prerender_ blank $ performEvent_ $ evClick $> liftJSM
    ( currentWindow >>= \case
        Nothing -> pure ()
        Just win -> scrollTo win 0 0
    )
  pure a
