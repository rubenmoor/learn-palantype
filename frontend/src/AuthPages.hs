{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AuthPages
    ( login
    , signup
    )
where

import           Reflex.Dom                     (leftmost, keypress,  Key (Enter),  attachWith
                                                , dyn_
                                                , widgetHold_
                                                , PostBuild
                                                , Dynamic
                                                , elClass
                                                , gate
                                                , blank
                                                , current
                                                , tag
                                                , inputElementConfig_setValue
                                                , zipDyn
                                                , holdDyn
                                                , EventWriter
                                                , MonadHold
                                                , Prerender
                                                , text
                                                , el
                                                , DomBuilder
                                                , updated
                                                , InputElement(..)
                                                , inputElementConfig_elementConfig
                                                , elementConfig_modifyAttributes
                                                , (=:)
                                                )
import           Obelisk.Route.Frontend         ( setRoute
                                                , pattern (:/)
                                                , SetRoute
                                                , R
                                                , RouteToUrl
                                                , routeLink
                                                )
import           Common.Route                   ( FrontendRoute
                                                    ( FrontendRoute_Auth
                                                    )
                                                , FrontendRoute_AuthPages
                                                    ( AuthPage_SignUp
                                                    )
                                                , FrontendRoute
                                                )
import           Control.Monad.Writer.Strict    ( MonadFix )
import           Data.Semigroup                 ( Endo )
import           Data.Generics.Product          ( field )
import           Data.Witherable                ( filter )
import           Control.Category               ( (<<<)
                                                , Category((.))
                                                )
import           Data.Function                  ( const
                                                , ($)
                                                , (&)
                                                )
import           Data.Functor                   ( (<&>)
                                                , void
                                                , (<$>)
                                                , ($>)
                                                )
import           Data.Bool                      ( not
                                                , Bool(..)
                                                , bool
                                                )
import           Data.Eq                        ( (/=) )
import           Data.Either                    ( either
                                                , isRight
                                                , Either(..)
                                                )
import           Data.Default                   ( Default(def) )
import           Client                         (postAuthenticate
                                                , postAuthNew
                                                , request
                                                , postDoesUserExist
                                                )
import           Data.Maybe                     ( isNothing
                                                , fromMaybe
                                                , maybe
                                                , Maybe(..)
                                                )
import           Control.Lens.Setter            ( (?~)
                                                , (.~)
                                                )
import qualified Data.Text                     as Text

import           Shared                         ( elLabelInput
                                                , btnSubmit
                                                , elLabelCheckbox
                                                )
import           State                          (State(..)
                                                , Session(..)
                                                , updateState
                                                )
import           Common.Auth                    ( LoginData(..)
                                                , UserNew(UserNew)
                                                )
import           Control.Monad.Reader           ( ask
                                                , MonadReader
                                                )
import           Data.Char                      ( isAlphaNum )
import           Data.Witherable                ( Filterable
                                                    ( mapMaybe
                                                    , catMaybes
                                                    )
                                                )
import           Common.Model                   (Message(..) )

signup
    :: forall t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadHold t m
       , MonadFix m
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       , MonadReader (Dynamic t State) m
       )
    => m ()
signup = elClass "div" "auth" $ mdo

    dynState <- ask

    el "h1" $ text "Sign up"

    (dynMUserName, inputUserName) <- elLabelInput def "User name" 64 "username"

    let evPressEnter = keypress Enter inputUserName
    dynUserExists                 <- holdDyn False evUserExists
    dyn_ $ dynUserExists <&> \userExists -> if userExists
        then elClass "p" "red small" (text "This user name already exists.")
        else blank

    let behNotAlphanumeric = current $ dynMUserName <&> \case
            Just str -> Text.filter (not <<< isAlphaNum) str /= ""
            Nothing  -> False
    dynNotAlphaNumeric <- holdDyn False
        $ attachWith (\na _ -> na) behNotAlphanumeric evFocusLost
    dyn_ $ dynNotAlphaNumeric <&> \notAlphaNumeric -> if notAlphaNumeric
        then elClass "p" "red small"
            $ text "The user name can only contain A-Z, a-z, 0-9."
        else blank

    el "p"
        $ text
              "Your user name must contain only alphanumeric characters \
                  \and it won't be publicly visible."

    let evFocusLost =
            void $ filter not $ updated $ _inputElement_hasFocus inputUserName
        eUserName = maybe (Left "user empty") Right <$> dynMUserName

    evUserExists <- mapMaybe (either (const Nothing) Just)
        <$> request (postDoesUserExist eUserName evFocusLost)

    dynDefaultAlias <- holdDyn True $ _inputElement_input inputAlias $> False
    let evSetDefaultAlias =
            tag (Text.take 16 . fromMaybe "" <$> current dynMUserName)
                $ gate (current dynDefaultAlias) evFocusLost
        inputAliasConf = def & inputElementConfig_setValue .~ evSetDefaultAlias
    (dynMAlias, inputAlias) <- elLabelInput inputAliasConf "Alias" 16 "alias"

    el "p" $ text "Your alias is your public identity, maximum 16 characters."

    el "h3" $ text "Public visibility"
    dynCheckedVisible <- elLabelCheckbox False "Show my scores" "scores-visible"

    el "p"
        $ text
              "If you check this, your scores will be publicly visible. \
                  \If not, nothing will be shown, not even your alias."

    let conf =
            def
                &  inputElementConfig_elementConfig
                .  elementConfig_modifyAttributes
                .~ (   bool ("type" =: Just "text") ("type" =: Just "password")
                   <$> evCheckedHidePassword
                   )
    (dynMPassword, _) <- elLabelInput conf "Password" 64 "password"
    el "br" blank
    evCheckedHidePassword <- updated
        <$> elLabelCheckbox False "Hide password input" "hide-password"

    el "p"
        $ text
              "You enter your password only once. There are \
                    \no invalid passwords except for an empty one."

    el "hr" blank

    evSubmit <- btnSubmit $ text "Submit"

    let dynEUserNew =
          zipDyn dynState (
            zipDyn dynUserExists $
              zipDyn dynMUserName $
                zipDyn dynMPassword $
                  zipDyn dynMAlias dynCheckedVisible
            ) <&> \case
                (_, (True, _)) -> Left "User name already exists."
                (State{..}, (_, (Just u, (Just p, (mAlias, isVisible))))) ->
                    Right $ UserNew u p mAlias isVisible stApp
                _ -> Left "User name and password required."
    evRespNew <- request $ postAuthNew dynEUserNew $ leftmost [evSubmit, evPressEnter]
    updateState $ evRespNew <&> \case
        Left errMsg ->
            [field @"stApp" . field @"stMsg" ?~ Message "Error" errMsg]
        Right user ->
            [ field @"stSession" .~ SessionUser user
            , field @"stApp" . field @"stMsg" ?~ Message
                "Success"
                "Your account was registered successfully."
            ]

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

    dyn_ $ dynState <&> \State {..} ->
        setRoute $ filter isRight evRespNew $> stRedirectUrl

login
    :: forall t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Dynamic t State) m
       , PostBuild t m
       , Prerender t m
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       )
    => m ()
login = elClass "div" "auth" $ mdo

    dynState <- ask

    el "h1" $ text "Log in"

    (dynMUserName, _) <- elLabelInput def "User name" 64 "username"

    let conf =
            def
                &  inputElementConfig_elementConfig
                .  elementConfig_modifyAttributes
                .~ (   bool ("type" =: Just "text") ("type" =: Just "password")
                   <$> evCheckedHidePassword
                   )
    (dynMPassword, inputPassword) <- elLabelInput conf "Password" 64 "password"
    el "br" blank
    evCheckedHidePassword <- updated
        <$> elLabelCheckbox False "Hide password input" "hide-password"

    let evPressEnter = keypress Enter inputPassword
        evWrongInput =
            filter isNothing $ mapMaybe (either (const Nothing) Just) evRespAuth

    widgetHold_ blank $ evWrongInput $> elClass
        "p"
        "red small"
        (text "Wrong user name or password.")

    el "hr" blank
    evSubmit <- btnSubmit $ text "Submit"

    let dynELoginData = zipDyn dynMUserName dynMPassword <&> \case
            (Just ldUserName, Just ldPassword) -> Right LoginData { .. }
            _ -> Left "User name and password required."
    evRespAuth <- request $ postAuthenticate dynELoginData $ leftmost [evSubmit, evPressEnter]
    updateState $ mapMaybe (either Just (const Nothing)) evRespAuth <&> \errMsg ->
        [field @"stApp" . field @"stMsg" ?~ Message "Error" errMsg]
    let evLogin = catMaybes $ mapMaybe (either (const Nothing) Just) evRespAuth
    updateState $ evLogin <&> \(sd, appState) ->
      [ field @"stSession" .~ SessionUser sd
      , field @"stApp" .~ appState
      ]

    dyn_ $ dynState <&> \State {..} ->
        setRoute $ evLogin $> stRedirectUrl

    el
        "p"
        do
            text "Don't have an account yet? "
            routeLink (FrontendRoute_Auth :/ AuthPage_SignUp :/ ())
                $ text "Sign up"
            text " here."
