{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}

module PageWordList where
import Reflex.Dom (blank, DomBuilder (..), InputElementConfig, Reflex (Dynamic, current), elAttr, inputElementConfig_elementConfig, (=:), text, elementConfig_initialAttributes, InputElement (..), el, inputElementConfig_initialValue, elAttr', HasDomEvent (..), EventName (..), Prerender, elClass, widgetHold_, MonadHold (..), leftmost, dyn_, PostBuild, gate)
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Lens ((&), (%~), At (at), (?~), non, (.~), (<&>))
import Data.Default (Default(..))
import Client (request, getWordList)
import Control.Category ((<<<))
import TextShow (TextShow(showt))
import Text.Read (readMaybe)
import Shared (iFa)
import Data.Functor (($>))
import Control.Monad.Fix (MonadFix)

pageWordList
  :: forall t (m :: * -> *)
  . ( DomBuilder t m
    , PostBuild t m
    , Prerender t m
    , MonadFix m
    , MonadHold t m
    )
  => m ()
pageWordList = elClass "div" "max-w-5xl mx-auto mt-8" mdo
  elClass "h1" "text-4xl font-bold my-4" $ text "Frequent words generation"
  elClass "p" "my-2" do
    text "Search a list of "
    elAttr "a" ("href" =: "")
      $ text "2 Mio. words of the German language for words"
    text " that only contain the specified letters. Frequency information is \
        \provided with friendly permission by the Natural Langauge Processing \
        \Group Uni Leipzig. It is generated out of a "
    elAttr "a" ("href" =: "https://wortschatz.uni-leipzig.de/en")
      $ text "corpus of 35 Million sentences"
    text "."

  let
      setClass = inputElementConfig_elementConfig
               . elementConfig_initialAttributes
               . at "class" . non "" %~ (<> " rounded p-1 my-1 mr-2 w-full")
      confLetters = def & setClass
                 & inputElementConfig_initialValue .~ "asdfjklö"
  dynMLetters <- elLabelInput confLetters "Set of allowed letters" "letters"

  el "br" blank

  let
      confMax = def & setClass
                    & inputElementConfig_initialValue .~ "0"
  dynMMaxNumber <- elLabelInputNumber confMax "Limit the number of words in the response" "letters"
  elClass "p" "my-2" $ text "Put 0 to disable the limit."

  (e, _) <- elAttr' "button"
    (  "type"  =: "submit"
    <> "class" =: "rounded bg-grayishblue-800 text-white p-1 \
                  \focus:outline-white focus:outline-1 \
                  \focus:shadow-[0_0_20_0_rgba(13,83,181,0.8)] focus:shadow-grayishblue-800"
    ) $ text "Submit"

  let
      evSubmit = domEvent Click e
      dynELetter = maybe (Left "no input") (Right <<< Text.unpack) <$> dynMLetters
      dynEMax = maybe (Right 0) Right <$> dynMMaxNumber

  evResp <- request $ getWordList dynELetter dynEMax
    $ gate (not <$> current dynLoading) evSubmit

  dynLoading <- holdDyn False $ leftmost [evSubmit $> True, evResp $> False]
  dyn_ $ dynLoading <&> \case
    True -> elAttr "span"
      (  "title" =: "Waiting for response ..."
      <> "class" =: "mx-2"
      ) $ iFa "fas fa-spinner fa-spin"
    False -> blank


  el "br" blank
  el "br" blank

  widgetHold_ blank $ evResp <&> \case
    Left str -> elClass "p" "text-red-500" $ text str
    Right str -> elAttr "textarea"
      (  "class" =: "w-full border border-solid p-1 border-grayishblue-900 overflow-y-hidden"
      <> "readonly" =: "readonly"
      <> "rows"     =: showt (length $ Text.lines str)
      )
                 $ text str
  blank

elLabelInput
    :: DomBuilder t m
    => InputElementConfig e t (DomBuilderSpace m)
    -> Text
    -> Text
    -> m ( Dynamic t (Maybe Text))
elLabelInput conf label elemId = do
    elAttr "label" ("for" =: elemId <> "class" =: "block mt-2 text-xl font-bold") $ text label
    i <- inputElement $
      conf & inputElementConfig_elementConfig
           . elementConfig_initialAttributes
           %~ (at "id" ?~ elemId)
           . (at "type" ?~ "text")
           . (at "maxlength" ?~ "64")
    pure $ _inputElement_value i <&> \str ->
      if Text.null str then Nothing else Just str

elLabelInputNumber
    :: DomBuilder t m
    => InputElementConfig e t (DomBuilderSpace m)
    -> Text
    -> Text
    -> m ( Dynamic t (Maybe Int))
elLabelInputNumber conf label elemId = do
    elAttr "label" ("for" =: elemId <> "class" =: "block mt-2 text-xl font-bold") $ text label
    i <- inputElement $
      conf & inputElementConfig_elementConfig
           . elementConfig_initialAttributes
           %~ (at "id" ?~ elemId)
           . (at "type" ?~ "text")
           . (at "maxlength" ?~ "7")
    pure $ _inputElement_value i <&> \str ->
      if Text.null str
      then Nothing
      else readMaybe $ Text.unpack str
