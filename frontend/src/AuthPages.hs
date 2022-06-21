{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AuthPages
  ( login
  , signup
  ) where

import           Reflex.Dom                     (elClass, gate, blank, current, tag, inputElementConfig_setValue, zipDyn, holdDyn, EventWriter, MonadHold, Prerender
                                                , text
                                                , el
                                                , DomBuilder
                                                , updated
                                                , InputElement (..)
                                                , inputElementConfig_elementConfig
                                                , elementConfig_modifyAttributes
                                                , (=:)
                                                )
import Obelisk.Route.Frontend (setRoute, pattern (:/), SetRoute, R, RouteToUrl, routeLink)
import Common.Route (FrontendRoute(FrontendRoute_Auth), FrontendRoute_AuthPages(AuthPage_SignUp), FrontendRoute)
import Control.Monad.Writer.Strict (MonadFix)
import Data.Semigroup (Endo)
import Data.Generics.Product (field)
import Data.Witherable (filter)
import Control.Category (Category ((.)))
import Data.Function (($), (&))
import Data.Functor ((<&>), void, (<$>), ($>))
import Data.Bool (not, Bool (..), bool)
import Data.Eq ((==))
import Data.Either (Either (..))
import Data.Default (Default(def))
import Client (postAuthNew, request, postDoesUserExist)
import Data.Maybe (fromMaybe, maybe, Maybe (..))
import Control.Lens.Setter ((?~), (.~))
import qualified Data.Text as Text

import Shared (elLabelInput, btnSubmit, elLabelCheckbox)
import State (Session(SessionUser), State, updateState, Message (..))
import Common.Auth (UserNew(UserNew))

signup
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , SetRoute t (R FrontendRoute) m
  , RouteToUrl (R FrontendRoute) m
  , Prerender t m
  , MonadHold t m
  , MonadFix m
  , EventWriter t (Endo State) m
  ) =>  m ()
signup = elClass "div" "auth" $ mdo
    -- -- TODO: store current route in route /register/ to allow going back
    -- setRoute $ mapMaybe reqSuccess eResponse $> FrontendRoute_AliasRename :/ ()

    el "h1" $ text "Sign up"

    (dynMUserName, inputUserName) <- elLabelInput def "User name" "username"

    el "p" $ text "Your user name must contain only alphanumeric characters \
                  \and it won't be publicly visible."

    let evFocusLost = void $ filter not $ updated $ _inputElement_hasFocus inputUserName
        eUserName = maybe (Left "user empty") Right <$> dynMUserName
    evUserExists <- void . filter (== Right True) <$> request (postDoesUserExist eUserName evFocusLost)
    updateState $ evUserExists $> [field @"stMsg" ?~ Message "Error" "username already exists"]

    dynDefaultAlias <- holdDyn True $ _inputElement_input inputAlias $> False
    let evSetDefaultAlias =
          tag (Text.take 16 . fromMaybe "" <$> current dynMUserName) $
              gate (current dynDefaultAlias) evFocusLost
        inputAliasConf = def & inputElementConfig_setValue .~ evSetDefaultAlias
    (dynMAlias, inputAlias) <- elLabelInput inputAliasConf "Alias" "alias"

    el "p" $ text "Your alias is your public identity, maximum 16 characters."

    el "h3" $ text "Public visibility"
    dynCheckedVisible <- elLabelCheckbox False "Show my scores" "scores-visible"

    el "p" $ text "If you check this, your scores will be publicly visible. \
                  \If not, nothing will be shown, not even your alias."

    let conf = def
          & inputElementConfig_elementConfig
          . elementConfig_modifyAttributes
          .~ (bool ("type" =: Just "text") ("type" =: Just "password") <$> evCheckedHidePassword)
    (dynMPassword, _) <- elLabelInput conf "Password" "password"
    el "br" blank
    evCheckedHidePassword <- updated <$> elLabelCheckbox False "Hide password input" "hide-password"

    el "p" $ text "You enter your password only once. There are \
                    \no invalid passwords except for an empty one."

    el "hr" blank

    eSend <- btnSubmit $ text "Submit"

    dynUserExists <- holdDyn False (evUserExists $> True)
    let dynEUserNew = zipDyn dynUserExists (zipDyn dynMUserName $ zipDyn dynMPassword $ zipDyn dynMAlias dynCheckedVisible) <&> \case
          (True, _                                      ) -> Left "User name already exists."
          (_   , (Just u, (Just p, (mAlias, isVisible)))) -> Right $ UserNew u p mAlias isVisible
          _                                               -> Left "User name and password required."
    evRespNew <- request $ postAuthNew dynEUserNew eSend
    updateState $ evRespNew <&> \case
        Left  errMsg -> [field @"stMsg"     ?~ Message     "Error" errMsg ]
        Right user   -> [field @"stSession" .~ SessionUser user           ]

    el "h2" $ text "Why register?"

    el "p"
        $ text
              "You don't have to register to practice on Palantype.com. \
         \For an unregistered, anonymous user the progress and configuration \
         \data is stored in the local storage of your browser. \
         \At any time, you can delete all the data \
         \by clearing your browser data. The only downside: In case you visit \
         \Palantype.com from another browser (e.g. on a different computer), \
         \you won't have access to your data."

    el "p"
        $ text
              "As soon as you create an account, your progress and configuration \
         \data will be stored on the Palantype.com server. \
         \You can access your account from any computer and any browser to \
         \continue where you left off. \
         \In order to delete all data related to your account, there is a \
         \delete option right on this website."

    -- setRoute $ evRespNew $>134

login
    :: forall t (m :: * -> *)
     . ( DomBuilder t m
       -- , EventWriter t (Endo State) m
       -- , SetRoute t (R FrontendRoute) m
       -- , MonadReader (Dynamic t State) m
       , Prerender t m
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       )
    => m ()
login = do

    el "p" do
      text "Don't have an account yet? "
      routeLink (FrontendRoute_Auth :/ AuthPage_SignUp :/ ()) $ text "Sign up"
      text " here."

    el "p"
        $ text
              "As soon as you create an account, your progress and configuration \
         \data will be stored on the Palantype.com server. \
         \You can access your account from any computer and any browser to \
         \continue where you left off. \
         \In order to delete all data related to your account, there is a \
         \delete option right on this website."
