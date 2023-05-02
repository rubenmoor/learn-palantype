{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}

module PageWordList
    ( pageWordList
    ) where

import           Client                         ( getWordList
                                                , postRender
                                                , request
                                                )
import           Control.Lens                   ( (%~)
                                                , (&)
                                                , (.~)
                                                , (<&>)
                                                , (?~)
                                                , At(at)
                                                , non
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Random           ( evalRand
                                                , newStdGen
                                                )
import           Control.Monad.Random.Strict    ( StdGen )
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Lazy          as Lazy
import           Data.Default                   ( Default(..) )
import           Data.Foldable                  ( for_ )
import           Data.Functor                   ( ($>) )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           Reflex.Dom                     ( (=:)
                                                , DomBuilder(..)
                                                , EventName(..)
                                                , HasDomEvent(..)
                                                , InputElement(..)
                                                , InputElementConfig
                                                , MonadHold(..)
                                                , PerformEvent(..)
                                                , PostBuild(..)
                                                , Prerender
                                                , Reflex(..)
                                                , SelectElement(..)
                                                , SelectElementConfig
                                                , attach
                                                , blank
                                                , dynText
                                                , dyn_
                                                , el
                                                , elAttr
                                                , elAttr'
                                                , elClass
                                                , elementConfig_initialAttributes
                                                , gate
                                                , inputElementConfig_elementConfig
                                                , inputElementConfig_initialChecked
                                                , inputElementConfig_initialValue
                                                , leftmost
                                                , selectElementConfig_elementConfig
                                                , selectElementConfig_initialValue
                                                , text
                                                , widgetHold_
                                                , zipDyn
                                                )
import           Shared                         ( iFa )
import           System.Random.Shuffle          ( shuffleM )
import           Text.Read                      ( readMaybe )
import           TextShow                       ( TextShow(..) )

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

  evStdGen <- postRender do
      ePb <- getPostBuild
      performEvent $ ePb $> liftIO newStdGen
  behMStdGen <- current <$> holdDyn Nothing (Just <$> evStdGen)

  elClass "h1" "text-4xl font-bold my-4" $ text "Frequent words generation"
  elClass "p" "my-2" do
    text "Search a list of "
    elAttr "a" ("href" =: "https://sourceforge.net/projects/germandict/files/")
      $ text "2 Mio. words of the German language"
    text " for words that only contain the specified letters. Frequency \
         \information is provided with friendly permission by the Natural \
         \Langauge Processing Group Uni Leipzig. It is generated out of a "
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
  elClass "p" "my-2" $ text "Put 0 to disable the limit. The result will include \
                            \all valid words sorted by highest frequency first."

  dynCaseinsensitive <- elLabelCheckbox def "checkbox-caseinsensitive" True
    $ el "span" $ text "Case insensitive, e.g. 'a' is treated like 'A'"

  el "br" blank

  (e, _) <- elAttr' "button"
    (  "type"  =: "submit"
    <> "class" =: "rounded bg-grayishblue-800 text-white p-1 mt-2 \
                  \focus:outline-white focus:outline-1 \
                  \focus:shadow-[0_0_20_0_rgba(13,83,181,0.8)] focus:shadow-grayishblue-800"
    ) $ text "Submit"

  let
      evSubmit = domEvent Click e
      dynELetter = maybe (Left "no input") Right <$> dynMLetters
      dynEMax = maybe (Right 0) Right <$> dynMMaxNumber

  evResp <- request $ getWordList dynELetter dynEMax dynCaseinsensitive
    $ gate (not <$> current dynLoading) evSubmit

  dynLoading <- holdDyn False $ leftmost [evSubmit $> True, evResp $> False]
  dyn_ $ dynLoading <&> \case
    True -> elAttr "span"
      (  "title" =: "Waiting for response ..."
      <> "class" =: "mx-2"
      ) $ iFa "fas fa-spinner fa-spin"
    False -> blank

  el "br" blank

  widgetHold_ blank $ attach behMStdGen evResp <&> \(mStdGen, resp) -> case resp of
    Left str -> elClass "p" "text-red-500" $ text str
    Right ls -> elResult ls mStdGen
  blank

elResult
  :: forall t (m :: * -> *)
  . ( DomBuilder t m
    , PostBuild t m
    )
  => [Text]
  -> Maybe StdGen
  -> m ()
elResult ls mStdGen = do
    elClass "h2" "text-3xl font-bold text-grayishblue-900 mt-4 mb-2" $ text "Result"
    dynFormat <- elLabelSelect def "Output format" "formats"
      [ SeparatorNewline
      , SeparatorSpace
      , SeparatorComma
      , FormatJson
      ]

    el "br" blank

    dynShuffle <- elLabelCheckbox def "checkbox-shuffle" False
      $ text "Shuffle words"

    el "br" blank

    elAttr "textarea"
      (  "class" =: "w-full border border-solid p-1 border-grayishblue-900 mt-2"
      <> "readonly" =: "readonly"
      <> "rows"     =: "24"
      ) $ dynText $ zipDyn dynFormat dynShuffle <&> \(format, bShuffle) ->
          let ls' = case (bShuffle, mStdGen) of
                (True, Just stdGen) -> evalRand (shuffleM ls) stdGen
                (False, _         ) -> ls
                (_    , Nothing   ) -> ls
          in  case format of
                SeparatorNewline -> Text.unlines ls'
                SeparatorSpace   -> Text.unwords ls'
                SeparatorComma   -> Text.intercalate "," ls'
                FormatJson       -> Text.decodeUtf8 $ Lazy.toStrict $ Aeson.encode ls'

data ResultFormat = SeparatorNewline | SeparatorSpace | SeparatorComma | FormatJson
  deriving (Read, Show)

instance TextShow ResultFormat where
  showb = \case
    SeparatorNewline -> "Separator: newline"
    SeparatorSpace   -> "Separator: space"
    SeparatorComma   -> "Separator: comma"
    FormatJson       -> "JSON array"

elLabelSelect
    :: forall e t (m :: * -> *) v
    . (DomBuilder t m
      , Read v
      , Show v
      , TextShow v
      )
    => SelectElementConfig e t (DomBuilderSpace m)
    -> Text
    -> Text
    -> [v]
    -> m (Dynamic t v)
elLabelSelect conf label elemId values = do
    elAttr "label" ("for" =: elemId <> "class" =: "block mt-2 text-xl font-bold") $ text label
    let conf' = conf
          & selectElementConfig_elementConfig
              . elementConfig_initialAttributes
                  .~ ( "id" =: elemId
                    <> "class" =: "py-1 pl-1 pr-8 my-1 rounded border-grayishblue-900 bg-zinc-200"
                     )
          & selectElementConfig_initialValue
              .~ Text.pack (show $ head values)
    (s, _) <- selectElement conf' $ for_ values \value ->
      elAttr "option" ("value" =: Text.pack (show value)) $ text $ showt value
    pure $ _selectElement_value s <&> \str ->
      fromMaybe (head values) $ readMaybe $ Text.unpack str

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

elLabelCheckbox
  :: forall er t (m :: * -> *)
  . ( DomBuilder t m
    )
  => InputElementConfig er t (DomBuilderSpace m)
  -> Text
  -> Bool
  -> m ()
  -> m (Dynamic t Bool)
elLabelCheckbox conf elemId bInitial label = do
  cb <-
      inputElement
      $  conf
      &  inputElementConfig_elementConfig
      .  elementConfig_initialAttributes
      .~ "type" =: "checkbox" <> "id" =: elemId
      & inputElementConfig_initialChecked .~ bInitial
  elAttr "label" ("for" =: elemId) label
  pure $ _inputElement_checked cb
