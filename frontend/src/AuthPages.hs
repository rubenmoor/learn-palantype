{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module AuthPages
    ( login
    , signup
    , settings
    )
where

import           Client                         ( getAuthData
                                                , postAliasRename
                                                , postAliasVisibility
                                                , postAuthNew
                                                , postAuthenticate
                                                , postDoesAliasExist
                                                , postDoesUserExist
                                                , request
                                                )
import           Common.Route                   ( FrontendRoute
                                                    ( FrontendRoute_Auth
                                                    )
                                                , FrontendRoute_AuthPages
                                                    ( AuthPage_SignUp
                                                    )
                                                )
import           Control.Applicative            ( pure )
import           Control.Category               ( (<<<)
                                                , Category((.))
                                                )
import           Control.Lens.Setter            ( (.~)
                                                , (?~)
                                                )
import           Control.Monad.Writer.Strict    ( MonadFix )
import           Data.Bool                      ( Bool(..)
                                                , bool
                                                , not
                                                )
import           Data.Default                   ( Default(def) )
import           Data.Either                    ( Either(..)
                                                , either
                                                , isRight
                                                )
import           Data.Eq                        ( (/=)
                                                , (==)
                                                )
import           Data.Function                  ( ($)
                                                , (&)
                                                , const
                                                )
import           Data.Functor                   ( ($>)
                                                , (<$>)
                                                , (<&>)
                                                , fmap
                                                , void
                                                )
import           Data.Generics.Product          ( field )
import           Data.Generics.Sum              ( _As )
import           Data.Maybe                     ( Maybe(..)
                                                , fromMaybe
                                                , isNothing
                                                , maybe
                                                )
import           Data.Semigroup                 ( Endo
                                                , Semigroup((<>))
                                                )
import qualified Data.Text                     as Text
import Data.Text (Text)
import           Obelisk.Route.Frontend         ( pattern (:/)
                                                , R
                                                , RouteToUrl
                                                , SetRoute
                                                )
import           Reflex.Dom                     ( (=:)
                                                , DomBuilder (DomBuilderSpace)
                                                , Dynamic
                                                , Event
                                                , EventName(Click)
                                                , EventWriter
                                                , InputElement(..)
                                                , Key(Enter)
                                                , MonadHold
                                                , PostBuild
                                                , Prerender
                                                , attachWith
                                                , blank
                                                , current
                                                , domEvent
                                                , dyn_
                                                , el
                                                , elAttr
                                                , elAttr'
                                                , elClass
                                                , elementConfig_initialAttributes
                                                , elementConfig_modifyAttributes
                                                , fanEither
                                                , gate
                                                , holdDyn
                                                , inputElement
                                                , inputElementConfig_elementConfig
                                                , inputElementConfig_initialChecked
                                                , inputElementConfig_setValue
                                                , keypress
                                                , leftmost
                                                , tag
                                                , tagPromptlyDyn
                                                , text
                                                , updated
                                                , widgetHold_
                                                , zipDyn, TriggerEvent, PerformEvent (Performable), InputElementConfig, holdUniqDyn
                                                )
import           Witherable                     ( Filterable
                                                    ( catMaybes
                                                    , mapMaybe
                                                    )
                                                , filter
                                                )

import           Common.Auth                    ( LoginData(..)
                                                , UserNew(UserNew)
                                                )
import           Common.Model                   ( Message(..) )
import           Control.Lens                   ( preview
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , ask
                                                )
import           Data.Char                      ( isAlphaNum )
import           Shared                         ( elLabelCheckbox
                                                , elLoginSignup
                                                , iFa', elRouteLink, setRouteAndLoading
                                                )
import           State                          ( Session(..)
                                                , State(..)
                                                , updateState, GetLoadedAndBuilt
                                                )
import Control.Monad.IO.Class (MonadIO)
import Data.Int (Int)
import TextShow (TextShow(..))

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
signup = elAttr "section" ("id" =: "user-form") $ mdo

    dynState <- ask

    el "h1" $ text "Sign up"

    (dynMUserName, inputUserName) <- elLabelInput def "User name" 64 "username"

    dynUserExists                 <- holdDyn False evUserExists
    dyn_ $ dynUserExists <&> \userExists -> if userExists
        then elFormMessage $ text "This user name already exists."
        else blank

    let behNotAlphanumeric = current $ dynMUserName <&> \case
            Just str -> Text.filter (not <<< isAlphaNum) str /= ""
            Nothing  -> False
    dynNotAlphaNumeric <- holdDyn False
        $ attachWith const behNotAlphanumeric evFocusLostUser
    dyn_ $ dynNotAlphaNumeric <&> \notAlphaNumeric -> if notAlphaNumeric
        then elFormMessage $ text "The user name can only contain A-Z, a-z, 0-9."
        else blank

    el "p" $ text
        "Your user name must contain only alphanumeric characters and it won't \
        \be publicly visible."

    let evFocusLostUser =
            void $ filter not $ updated $ _inputElement_hasFocus inputUserName
        eUserName = maybe (Left "user empty") Right <$> dynMUserName

    (_, evUserExists) <- fanEither <$> request (postDoesUserExist eUserName evFocusLostUser)

    dynDefaultAlias <- holdDyn True $ _inputElement_input inputAlias $> False
    let evSetDefaultAlias =
            tag (Text.take 16 . fromMaybe "" <$> current dynMUserName)
                $ gate (current dynDefaultAlias) evFocusLostUser
        inputAliasConf = def & inputElementConfig_setValue .~ evSetDefaultAlias
    (dynMAlias, inputAlias) <- elLabelInput inputAliasConf "Alias" 16 "alias"

    dynAliasExists <- holdDyn False evAliasExists
    dyn_ $ dynAliasExists <&> \aliasExists ->
      if aliasExists
      then elFormMessage $ text "This alias is already in use."
      else blank

    el "p" $ text "Your alias is your public identity, maximum 16 characters."

    let evFocusLostAlias =
            void $ filter not $ updated $ _inputElement_hasFocus inputAlias
        eAlias = maybe (Left "alias empty") Right <$> dynMAlias
    (_, evAliasExists) <- fanEither <$> request (postDoesAliasExist eAlias evFocusLostAlias)

    el "h3" $ text "Public visibility"
    dynCheckedVisible <- elLabelCheckbox False "Show my scores" "scores-visible"

    el "p" $ text "If you check this, your scores will be publicly visible. \
                  \If not, nothing will be shown, not even your alias."

    let conf =
          def &  inputElementConfig_elementConfig
              .  elementConfig_modifyAttributes
              .~ (   bool ("type" =: Just "text") ("type" =: Just "password")
                 <$> evCheckedHidePassword
                 )
    (dynMPassword, inputPassword) <- elLabelInput conf "Password" 64 "password"
    let evPressEnter = keypress Enter inputPassword
    el "br" blank
    evCheckedHidePassword <- updated
        <$> elLabelCheckbox False "Hide password input" "hide-password"

    el "p" $ text "You enter your password only once. There are \
                  \no invalid passwords except for an empty one."

    el "hr" blank

    evSubmit <- elButtonSubmit $ text "Submit"

    dynEUserNew <- holdUniqDyn $
          zipDyn dynState (
            zipDyn dynUserExists $
              zipDyn dynMUserName $
                zipDyn dynAliasExists $
                  zipDyn dynMPassword $
                    zipDyn dynMAlias dynCheckedVisible
            ) <&> \case
                (_, (True , _             )) -> Left "User name already exists."
                (_, (False, (_, (True, _)))) -> Left "Alias already in use."
                (State{..}, (False, (Just u, (False, (Just p, (mAlias, isVisible)))))) ->
                    Right $ UserNew u p mAlias isVisible stApp
                (_, (_, (Nothing, _))) -> Left "User name required."
                (_, (_, (_, (_, (Nothing, _))))) -> Left "Password required."

    evRespNew <- request $ postAuthNew dynEUserNew $ leftmost [evSubmit, evPressEnter]
    updateState $ evRespNew <&> \case
        Left errMsg ->
            [field @"stApp" . field @"stMsg" ?~ Message "Error" errMsg]
        Right sessionData ->
            [ field @"stSession" .~ SessionUser sessionData
            , field @"stApp" . field @"stMsg" ?~ Message
                "Success"
                "Your account was registered successfully."
            ]

    el "br" blank
    el "br" blank
    el "h2" $ text "Why register?"

    el "p" $ text
         "You don't have to register to practice on Palantype.com. \
         \For an unregistered, anonymous user the progress and configuration \
         \data is stored in the local storage of your browser. \
         \At any time, you can delete all the data \
         \by clearing your browser data. The only downside: In case you visit \
         \Palantype.com from another browser (e.g. on a different computer), \
         \you won't have access to your data."

    el "p" $ text
         "As soon as you create an account, your progress and configuration \
         \data will be stored on the Palantype.com server. \
         \You can access your account from any computer and any browser to \
         \continue where you left off. \
         \In order to delete all data related to your account, there is a \
         \delete option right on this website."

    dynRedirectUrl <- holdUniqDyn $ stRedirectUrl <$> dynState
    dyn_ $ dynRedirectUrl <&> \url -> setRouteAndLoading $ filter isRight evRespNew $> url

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
login = elAttr "section" ("id" =: "user-form") mdo

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
    evCheckedHidePassword <- el "p" $ updated
        <$> elLabelCheckbox False "Hide password input" "hide-password"

    let evPressEnter = keypress Enter inputPassword
        evWrongInput =
            filter isNothing $ mapMaybe (either (const Nothing) Just) evRespAuth

    widgetHold_ blank $ evWrongInput $> elFormMessage
      (text "Wrong user name or password.")

    el "hr" blank
    evSubmit <- elButtonSubmit $ text "Submit"

    let dynELoginData = zipDyn dynMUserName dynMPassword <&> \case
            (Just ldUserName, Just ldPassword) -> Right LoginData { .. }
            (Nothing, _) -> Left "User name required."
            (_, Nothing) -> Left "Password required."
    evRespAuth <- request $ postAuthenticate dynELoginData $ leftmost [evSubmit, evPressEnter]
    updateState $ mapMaybe (either Just (const Nothing)) evRespAuth <&> \errMsg ->
        [field @"stApp" . field @"stMsg" ?~ Message "Error" errMsg]
    let evLogin = catMaybes $ mapMaybe (either (const Nothing) Just) evRespAuth
    updateState $ evLogin <&> \(sd, appState) ->
      [ field @"stSession" .~ SessionUser sd
      , field @"stApp" .~ appState
      ]

    dynRedirectUrl <- holdUniqDyn $ stRedirectUrl <$> dynState
    dyn_ $ dynRedirectUrl <&> \url -> setRouteAndLoading $ evLogin $> url

    el "br" blank
    el "br" blank
    el "p" do
            text "Don't have an account yet? "
            elRouteLink (FrontendRoute_Auth :/ AuthPage_SignUp :/ ())
                $ text "Sign up"
            text " here."

settings
  :: forall t (m :: * -> *)
  . ( DomBuilder t m
    , EventWriter t (Endo State) m
    , MonadFix m
    , MonadHold t m
    , MonadIO (Performable m)
    , MonadReader (Dynamic t State) m
    , PerformEvent t m
    , PostBuild t m
    , Prerender t m
    , SetRoute t (R FrontendRoute) m
    , TriggerEvent t m
    )
  => GetLoadedAndBuilt t
  -> m ()
settings getLoadedAndBuilt = do

    dynState <- ask

    evLoadedAndBuilt <- getLoadedAndBuilt

    elClass "div" "shadow-md p-1" $ do
        elClass "div" "float-left" $ do
            domBack <-
              elClass "span" "text-zinc-500 hover:text-grayishblue-800 text-3xl \
                             \cursor-pointer"
              $ iFa' "fas fa-arrow-circle-left"
            behRedirectUrl <- fmap current $ holdUniqDyn $ stRedirectUrl <$> dynState
            setRouteAndLoading $ tag behRedirectUrl $ domEvent Click domBack
        dynRedirectUrl <- holdUniqDyn $ stRedirectUrl <$> dynState
        elLoginSignup dynRedirectUrl
        elClass "br" "clear-both" blank

    elAttr "section" ("id" =: "user-form") mdo
        el "h1"  $ text "Settings"

        dynCurrentAlias <- holdUniqDyn $ dynState <&> fromMaybe "" . preview
              (   field @"stSession"
                . _As @"SessionUser"
                . field @"sdAliasName"
              )
        let
            conf = def & inputElementConfig_setValue
              .~ tagPromptlyDyn dynCurrentAlias
                                (leftmost [evLoadedAndBuilt, void evRespAliasRenameFail])
        (dynMAliasNew, inputAliasNew) <- elLabelInput conf "Change alias" 64 "alias-new"

        let evFocusLostAlias =
              void $ filter not $ updated $ _inputElement_hasFocus inputAliasNew
            dynEAliasNew = zipDyn dynAliasExists (zipDyn dynMAliasNew dynCurrentAlias) <&> \case
              (True, _                         ) -> Left "alias already taken"
              (False, (Nothing      , _       )) -> Left "alias empty"
              (False, (Just aliasNew, aliasOld)) | aliasNew == aliasOld -> Left ""
              (False, (Just aliasNew, _       )) -> Right aliasNew

        (_, evAliasExists) <-
          fmap fanEither $ request $
            postDoesAliasExist dynEAliasNew
                               evFocusLostAlias

        let evPressEnter = keypress Enter inputAliasNew
        (domButton, _) <- elAttr' "button"
          (  "type"  =: "submit"
          <> "class" =: "ml-4 rounded-xl bg-grayishblue-800 text-white px-2 py-1"
          ) $ text "Submit"
        let evSubmitAlias = domEvent Click domButton
        dynAuthData <- holdUniqDyn $ getAuthData <$> dynState
        (evRespAliasRenameFail, evRespAliasRename) <-
          fanEither <$> request (postAliasRename dynAuthData
                                                 dynEAliasNew
                                                 (leftmost [evSubmitAlias, evPressEnter])
                                )
        widgetHold_ blank $ evRespAliasRename <&> \new -> el "p" do
          elClass "span" "text-green-500 text-lg w-4 inline-block" $ text "✓"
          text " Alias successfully changed to "
          elClass "span" "font-bold" $ text new

        dynAliasExists <- holdDyn False evAliasExists
        dyn_ $ dynAliasExists <&> \aliasExists ->
          if aliasExists
          then elFormMessage $ text "This alias is already in use."
          else blank

        updateState $ evRespAliasRename <&> \new ->
          [ field @"stSession" . _As @"SessionUser" . field @"sdAliasName" .~ new
          ]
        updateState $ evRespAliasRenameFail <&> \strErr ->
          [ field @"stApp" . field @"stMsg" ?~ Message "Error" strErr ]

        el "hr" blank

        el "h3" $ text "Public visibility"

        let elemId = "cb-visiblity"
        dynVisible <- holdUniqDyn $ dynState <&> fromMaybe False . preview
            (   field @"stSession"
              . _As @"SessionUser"
              . field @"sdAliasVisible"
            )
        dyn_ $ dynVisible <&> \bVisible -> do
          cb <-
              inputElement
              $  def
              &  inputElementConfig_elementConfig
              .  elementConfig_initialAttributes
              .~ "type" =: "checkbox" <> "id" =: elemId
              & inputElementConfig_initialChecked .~ bVisible
          elAttr "label" ("for" =: elemId) $ el "span" $ text "Show my scores"
          let dynShowScoresChecked = _inputElement_checked cb
              evShowScoresChecked = updated dynShowScoresChecked
          updateState $ evShowScoresChecked <&> \isChecked ->
            [ field @"stSession" . _As @"SessionUser" . field @"sdAliasVisible" .~ isChecked]
          void $ request $ postAliasVisibility dynAuthData
                                               (Right <$> dynShowScoresChecked)
                                               (void evShowScoresChecked)

        el "p" $ text
            "If you check this, your scores will be publicly visible. \
            \If not, nothing will be shown, not even your alias."

elFormMessage :: DomBuilder t m => m a -> m a
elFormMessage = elClass "p" "text-red-500 text-sm"

elButtonSubmit
  :: DomBuilder t m
  => m ()
  -> m (Event t ())
elButtonSubmit inner = do
    (e, _) <- elAttr' "button"
      (  "type"  =: "submit"
      <> "class" =: "rounded-2xl text-2xl bg-grayishblue-800 text-white px-3 py-2 \
                    \focus:outline-white focus:outline-1 \
                    \focus:shadow-[0_0_20_0_rgba(13,83,181,0.8)] focus:shadow-grayishblue-800"
      ) inner
    pure $ domEvent Click e

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
elLabelInput conf strLabel maxlength id = do
    elAttr "label" ("for" =: id) $ el "h3" $ text strLabel
    i <-
        inputElement
        $  conf
        &  inputElementConfig_elementConfig
        .  elementConfig_initialAttributes
        .~ "id" =: id <> "type" =: "text" <> "maxlength" =: showt maxlength
    let dynStr  = _inputElement_value i
        dynMStr = dynStr <&> \s -> if Text.null s then Nothing else Just s
    pure (dynMStr, i)
