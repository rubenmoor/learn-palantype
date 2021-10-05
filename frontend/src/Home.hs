{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Home where

import           Client                      (postConfigNew, postRender)
import           Common.Alphabet             (PTChar (..), PTChord (..),
                                              mkPTChord, showChord, showKey, showLetter)
import           Common.Api                  (PloverCfg (..))
import           Common.Route                (FrontendRoute (FrontendRoute_Main))
import           Control.Applicative         (Applicative (..))
import           Control.Category            (Category ((.)))
import           Control.Lens.Setter         ((%~), (.~))
import           Control.Monad               ((=<<), unless, when, (<=<))
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Control.Monad.Reader        (MonadReader, asks)
import           Data.Bool                   (bool, Bool (..), not)
import           Data.Default                (Default (def))
import           Data.Either                 (Either (..))
import           Data.Foldable               (Foldable (foldl, null), concat)
import           Data.Function               (const, ($), (&))
import           Data.Functor                (fmap, void, ($>), (<$>), (<&>))
import           Data.Generics.Product       (field)
import           Data.List                   (elem, sort)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (Maybe (..), listToMaybe)
import           Data.Monoid                 (Monoid (mconcat, mempty), (<>))
import           Data.Proxy                  (Proxy (..))
import           Data.Semigroup              (Endo (..))
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.String                 (String, unwords)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Tuple                  (fst, snd)
import           Data.Witherable             (Filterable (catMaybes, mapMaybe))
import           GHCJS.DOM.EventM            (on)
import           GHCJS.DOM.FileReader        (getResult, load, newFileReader,
                                              readAsText)
import           GHCJS.DOM.HTMLElement       (focus)
import           GHCJS.DOM.Types             (File, HTMLInputElement (..), uncheckedCastTo)
import           Language.Javascript.JSaddle (FromJSVal (fromJSVal),
                                              ToJSVal (toJSVal), liftJSM)
import           Obelisk.Route.Frontend      (pattern (:/), R,
                                              SetRoute (setRoute))
import           Reflex.Dom                  (elDynClass, delay, DomBuilder (DomBuilderSpace, inputElement),
                                              DomSpace (addEventSpecFlags),
                                              EventName (Click, Keydown),
                                              EventResult, EventWriter,
                                              HasDomEvent (domEvent),
                                              InputElement (..),
                                              InputElementConfig,
                                              MonadHold (holdDyn),
                                              PerformEvent (performEvent_), prerender,
                                              PostBuild (getPostBuild),
                                              Prerender(Client),
                                              Reflex (Dynamic, Event, never, updated),
                                              blank, dyn, dyn_, el, el', elAttr,
                                              elClass, elClass', elDynAttr,
                                              elementConfig_eventSpec,
                                              elementConfig_initialAttributes,
                                              elementConfig_modifyAttributes,
                                              foldDyn,
                                              inputElementConfig_elementConfig,
                                              inputElementConfig_initialValue,
                                              inputElementConfig_setValue,
                                              keydown, keyup, leftmost,
                                              mergeWith, prerender_,
                                              preventDefault, splitDynPure,
                                              text, wrapDomEvent, switchDyn,
                                              (=:), _element_raw, _inputElement_element, _el_element)
import           Servant.Common.Req          (reqSuccess)
import           Shared                      (iFa, reqFailure, whenJust, dynSimple, prerenderSimple)
import           State                       (EStateUpdate, Message (..),
                                              State (..), updateState)
import Data.Witherable (Filterable(filter))
import Data.Eq (Eq((==)))

default (Text)

elFileInput
  :: DomBuilder t m
  => Event t Text
  -> m (Event t File)
elFileInput eSet = do
  i <- inputElement $ def
        & inputElementConfig_setValue .~ eSet
        & inputElementConfig_elementConfig
          . elementConfig_initialAttributes
          .~ ("type" =: "file" <> "accept" =: "text/cfg")

  let eFiles = _inputElement_files i
  pure $ mapMaybe listToMaybe $ updated eFiles

message
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , PostBuild t m
  , MonadReader (Dynamic t State) m
  , EventWriter t EStateUpdate m
  )
  => m ()
message = do
  dynMsg <- asks (stMsg <$>)
  dyn_ $ dynMsg <&> \mMsg -> whenJust mMsg $ \Message {..} ->
    let spanClose = elClass' "span" "close" $ iFa "fas fa-times"
    in  elClass "div" "msgOverlay" $ do
          (elClose, _) <- spanClose
          let eClose = domEvent Click elClose
          updateState $ eClose $> (field @"stMsg" .~ Nothing)
          el "div" $ text msgCaption
          el "span" $ text msgBody

if' :: Monoid a => Bool -> a -> a
if' True x  = x
if' False _ = mempty

settings
  :: forall t js (m :: * -> *).
  ( DomBuilder t m
  , PostBuild t m
  , Prerender js t m
  , SetRoute t (R FrontendRoute) m
  , EventWriter t EStateUpdate m
  , MonadReader (Dynamic t State) m
  , MonadFix m
  )
  => m ()
settings = do
  eFile <- elClass "div" "dropdown" $ do
    elClass "span" "dropdown-button" $ iFa "fas fa-cog"
    elClass "div" "dropdown-content" $ mdo
      eFile' <- elClass "span" "hiddenFileInput" $ do
        text "Upload your plover.cfg"
        elFileInput $ eReset $> ""
      eReset <- do
        (spanResetConfig, _) <- el' "span" $ text "Reset to default configuration"
        let eReset' = domEvent Click spanResetConfig
        updateState $ eReset $> (field @"stPloverCfg" .~ def)
        pure eReset'

      (e, _) <- el' "span" $ text "Reset progress"
      updateState $ domEvent Click e $> (field @"stProgress" .~ def)

      pure eFile'


  eReqResult <- postRender $ do
    fileReader <- liftJSM newFileReader
    let encoding = Just ("utf8" :: String)
    performEvent_ $ eFile <&> \f -> readAsText fileReader (Just f) encoding
    eText <- fmap catMaybes $
      wrapDomEvent fileReader (`on` load) $
        liftJSM $ do
          v <- getResult fileReader
          (fromJSVal <=< toJSVal) v
    dynEitherText <- holdDyn (Left "no file") (Right . Text.unpack <$> eText)
    postConfigNew dynEitherText (void eText)

  let eReqSuccess = mapMaybe reqSuccess eReqResult
      eReqFailure = mapMaybe reqFailure eReqResult

  updateState $
    eReqFailure <&> \str ->
      let msgCaption = "Error when loading file"
          msgBody = "Did you upload a proper .cfg file?\n" <> str
       in (field @"stMsg" .~ Just Message {..})
            . (field @"stPloverCfg" .~ def)

  let compatibleSystems =
        ["Palantype", "Possum Palantype", "Possum Palantype German"]
      isCompatible system = system `elem` compatibleSystems

  updateState $ eReqSuccess <&> \ploverCfg@PloverCfg {..} ->
    appEndo $ mconcat
      [ Endo $ field @"stPloverCfg" .~ ploverCfg
      , if' (not $ null pcfgUnrecognizedQwertys) $
          let msgCaption = "Unrecognized qwerty keys"
              msgBody =
                "Your key map contains unrecognized entries:\n"
                  <> Text.intercalate
                    "\n"
                    (Text.pack <$> pcfgUnrecognizedQwertys)
           in Endo $ field @"stMsg" .~ Just Message {..}
      , if' (not $ null pcfgUnrecognizedStenos) $
          let msgCaption = "Unrecognized steno keys"
              msgBody =
                "Your key map contains unrecognized entries:\n"
                  <> Text.intercalate
                    "\n"
                    (Text.pack <$> pcfgUnrecognizedStenos)
           in Endo $ field @"stMsg" .~ Just Message {..}
      , if' (not $ isCompatible pcfgSystem) $
          let msgCaption = "Incompatible system"
              msgBody =
                "Your system is " <> pcfgSystem
                  <> "\nCompatible systems at the moment are\n"
                  <> Text.intercalate "\n" compatibleSystems
           in Endo $ field @"stMsg" .~ Just Message {..}
      ]

  dynShowKeyboard <- asks (stShowKeyboard <$>)
  dyn_ $ dynShowKeyboard <&> \showKeyboard -> do
    (s, _) <-
      if showKeyboard
        then
          elClass' "span" "btnToggleKeyboard keyboardVisible" $
            iFa "far fa-keyboard"
        else
          elClass' "span" "btnToggleKeyboard keyboardHidden" $
            iFa "fas fa-keyboard"

    updateState $ domEvent Click s $> (field @"stShowKeyboard" %~ not)

  setRoute $ eReqSuccess $> FrontendRoute_Main :/ ()

data KeyState
  = KeyStateDown PTChar
  | KeyStateUp PTChar

stenoInput
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate (Client m)
  , MonadHold t m
  , MonadReader (Dynamic t State) m
  , PostBuild t m
  , Prerender js t m
  )
  => m (Event t PTChord)
stenoInput = do
  dynPloverCfg <- asks (stPloverCfg <$>)
  dynShowKeyboard <- asks (stShowKeyboard <$>)
  dynSimple $ dynPloverCfg <&> \PloverCfg {..} -> do
    prerenderSimple $ elClass "div" "stenoInput" $ mdo
      let keyChanges =
            pcfgLsKeySteno <&> \(qwertyKey, stenoKey) ->
              [ keydown qwertyKey kbInput $> [KeyStateDown stenoKey]
              , keyup   qwertyKey kbInput $> [KeyStateUp stenoKey]
              ]

          eKeyChange = mergeWith (<>) $ concat keyChanges

          register ::
            [KeyState] ->
            (Set PTChar, Set PTChar, Maybe PTChord) ->
            (Set PTChar, Set PTChar, Maybe PTChord)
          register es (keys, word, _) =
            let setKeys' = foldl accDownUp keys es
                (word', release') =
                  if Set.null setKeys'
                    then (Set.empty, Just $ mkPTChord word)
                    else (foldl accDown word es, Nothing)
             in (setKeys', word', release')
            where
              accDownUp s (KeyStateDown k) = Set.insert k s
              accDownUp s (KeyStateUp k)   = Set.delete k s

              accDown s (KeyStateDown k) = Set.insert k s
              accDown s (KeyStateUp _)   = s

      dynInput <- foldDyn register ( Set.empty
                                   , Set.empty
                                   , Nothing
                                   ) eKeyChange
      let (dynPressedKeys, dynChord) =
            splitDynPure $
              dynInput <&> \(keys, _, release) -> (keys, release)

      let dynClass = bool "displayNone" "" <$> dynShowKeyboard
      elDynClass "div" dynClass $
        elPTKeyboard pcfgMapStenoKeys dynPressedKeys pcfgSystem

      kbInput <- elStenoOutput dynPressedKeys

      -- TODO: doesn't seem to have the desired effect
      let eLostFocus = filter not $ updated $ _inputElement_hasFocus kbInput
      performEvent_ $ eLostFocus $> focus (_inputElement_raw kbInput)

      -- post build auto focus: the post build event happens before the element
      -- is mounted. postmount event waits for pull request to be accepted
      -- https://github.com/reflex-frp/reflex-dom-semui/issues/18
      ePb <- delay 0.1 =<< getPostBuild
      performEvent_ $ ePb $> focus (_inputElement_raw kbInput)

      let eChord = catMaybes $ updated dynChord
          chordSTFL = mkPTChord [LeftS, LeftT, LeftF, LeftL]
          eChordSTFL = filter (== chordSTFL) eChord
      updateState $ eChordSTFL $> (field @"stShowKeyboard" %~ not)

      pure eChord

elPTKeyboard
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , PostBuild t m
  )
  => Map PTChar [String]
  -> Dynamic t (Set PTChar)
  -> Text
  -> m ()
elPTKeyboard stenoKeys dynPressedKeys system =
  elClass "div" "keyboard" $ do
    el "table" $ do
      el "tr" $ do
        elCell LeftC "1"
        elCell LeftP "1"
        elCell LeftM "1"
        elCell LeftN "1"
        elAttr "td" ("colspan" =: "4" <> "class" =: "gap") blank
        elCell RightN "1"
        elCell RightM "1"
        elCell RightP "1"
        elCell RightH "1"
      el "tr" $ do
        elCell LeftS "1"
        elCell LeftT "1"
        elCell LeftF "1"
        elCell LeftL "1"
        elAttr "td" ("colspan" =: "3" <> "class" =: "gap") blank
        elCell RightE "1"
        elCell RightL "1"
        elCell RightF "1"
        elCell RightT "1"
        elCell RightS "1"
      el "tr" $ do
        elCell LeftCross "1"
        elCell LeftH "1"
        elCell LeftR "1"
        elCell LeftY "1"
        elCell LeftO "1"
        elCell MiddleI "2"
        elCell RightA "1"
        elCell RightC "1"
        elCell RightR "1"
        elCell RightCross "1"
        elCell RightPoint "1"
      el "tr" $ do
        elAttr "td" ("colspan" =: "4" <> "class" =: "gap") blank
        elCell LeftE "1"
        elCell LeftPipe "1"
        elCell RightPipe "1"
        elCell RightU "1"
    elClass "span" "system" $ text system
  where
    elCell cell colspan =
      let mQwertyKeys = Map.lookup cell stenoKeys

          showQwerties Nothing   = ""
          showQwerties (Just ks) = Text.pack $ unwords ks

          attrs =
            dynPressedKeys <&> \set' ->
              "colspan" =: colspan
                <> if Set.member cell set'
                  then "class" =: "pressed"
                  else mempty
       in if Map.member cell stenoKeys
            then elDynAttr "td" attrs $ do
              elClass "div" "steno " $ text $ showLetter cell
              elClass "div" "qwerty " $ text $ showQwerties mQwertyKeys
            else elAttr "td" ("colspan" =: colspan <> "class" =: "gap") blank

elStenoOutput
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , MonadFix m
  )
  => Dynamic t (Set PTChar)
  -> m (InputElement EventResult (DomBuilderSpace m) t)
elStenoOutput dynPressedKeys = mdo
  let eFocus =
        updated (_inputElement_hasFocus i) <&> \case
          True -> ("Type!", "class" =: Just "anthrazit")
          False -> ("Click me!", "class" =: Just "red")
      eTyping =
        updated dynPressedKeys <&> \pressedKeys ->
          if Set.null pressedKeys
            then ("...",                             "class" =: Nothing)
            else (showChord $ mkPTChord pressedKeys, "class" =: Nothing)
      eChange = leftmost [eFocus, eTyping]
      eSetValue = fst <$> eChange

  i <-
    inputElement $
      ( def :: InputElementConfig EventResult t (DomBuilderSpace m))
        & inputElementConfig_setValue .~ eSetValue
        & inputElementConfig_elementConfig
          . elementConfig_modifyAttributes
          .~ (snd <$> eChange)
        & inputElementConfig_initialValue .~ "Click me!"
        & inputElementConfig_elementConfig
          . elementConfig_initialAttributes
          .~ (  "readonly" =: "readonly"
             <> "autofocus" =: "autofocus"
             <> "class" =: "red"
             )
        & inputElementConfig_elementConfig
          . elementConfig_eventSpec
          %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m))
                               Keydown
                               (const preventDefault)
  pure i
