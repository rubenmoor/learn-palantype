{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BlockArguments #-}

module Home where

import Client
    (getMaybeAuthData, postEventViewPage, request,  postConfigNew,
      postRender,
    )
import Common.PloverConfig
    (defaultPloverSystemCfg, defaultPloverCfg,  CfgName (..),
      PloverSystemCfg (..),
      keyMapToPloverCfg,
      lsStenoQwerty,
      lsStenoQwertyOrig,
      lsStenoQwertz)
import Common.Api (showSymbol)
import Common.Route
    (showRoute,  FrontendRoute (..)
    , FrontendRoute_AuthPages (..)
    )
import Common.Stage (Stage (..), StageSpecialGeneric(..), StageIndex, mNext, mPrev)
import Palantype.Common.TH (readLoc, failure)
import Control.Applicative (Applicative (..))
import Control.Category
    ( (>>>), (<<<),
      Category ((.), id),
    )
import Control.Lens
    ((^.),  At (at),
      Ixed (ix),
      non,
      view,
    )
import Control.Lens.Setter
    ( (%~),
      (.~),
      (?~)
    )
import Control.Lens.Wrapped (_Wrapped')
import Control.Monad
    ( (<=<),
      (=<<),
      guard,
    )
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader
    (ask,  MonadReader,
      ReaderT,
      asks,
      withReaderT,
    )
import Data.Bool
    ((&&), (||), Bool (..),
      bool,
      not,
      otherwise,
    )
import Data.Default (Default (def))
import Data.Either (either, Either (..))
import Data.Eq (Eq ((==)))
import Data.Foldable
    (traverse_, Foldable (foldl, null),
      concat,
    )
import Data.Function
    ( ($),
      (&),
      const,
    )
import Data.Functor
    ( ($>),
      (<$>),
      (<&>),
      fmap,
      void,
    )
import Data.Functor.Misc (Const2 (Const2))
import Data.Generics.Product (field)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
    (maybe, fromMaybe,  Maybe (..),
      listToMaybe,
    )
import Data.Monoid ((<>))
import Data.Ord ((>=), Ord)
import Data.Proxy (Proxy (..))
import Data.Semigroup (Endo (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (String)
import Data.Text
    ( Text,
      unwords,
    )
import qualified Data.Text as Text
import Data.Tuple
    ( fst,
      snd,
    )
import Data.Witherable
    ( Filterable
          ( catMaybes,
            filter,
            mapMaybe
          ),
    )
import Data.Word (Word)
import GHC.Real (fromIntegral)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.FileReader
    ( getResult,
      load,
      newFileReader,
      readAsText,
    )
import GHCJS.DOM.HTMLElement (focus)
import GHCJS.DOM.Types (File)
import Language.Javascript.JSaddle
    ( FromJSVal (fromJSVal),
      ToJSVal (toJSVal),
      eval,
      liftJSM,
    )
import Obelisk.Generated.Static (static)
import Obelisk.Route.Frontend
    (Routed,  R,
      RouteToUrl,
      RoutedT,
      SetRoute (setRoute),
      askRoute,
      mapRoutedT,
      routeLink,
      pattern (:/),
    )
import Page.Common
    ( elFooter
    )
import Page.Introduction (introduction)
import Page.StageGeneric (getGenericExercise)
import qualified Page.Stage1 as Stage1
import qualified Page.Stage2 as Stage2
import Page.Stage15.PloverCommands (ploverCommands)
import Page.Stage15.Fingerspelling (fingerspelling)
import Page.Stage15.NumberMode (numberMode)
import Page.Stage15.CommandKeys (commandKeys)
import Page.Stage15.SpecialCharacters (specialCharacters)
import qualified Page.Patterns as Patterns
import Palantype.Common
    ( Chord (..),
      KeyIndex,
      Lang (..),
      Palantype (keyCode),
      fromChord,
      fromIndex,
      kiDown,
      kiInsert,
      kiUp,
      kiPageUp,
      kiPageDown,
      mkChord,
    )
import qualified Palantype.Common.Indices as KI
import qualified Palantype.DE as DE
import qualified Palantype.EN as EN
import Reflex.Dom
    (TriggerEvent, Performable, current, tag, attachPromptlyDynWithMaybe, zipDyn, inputElementConfig_initialChecked, (=:),
      DomBuilder
          ( DomBuilderSpace,
            inputElement
          ),
      DomSpace (addEventSpecFlags),
      EventName (Click, Keydown, Keyup),
      EventResult,
      EventSelector (select),
      EventTag (KeydownTag, KeyupTag),
      EventWriter,
      EventWriterT,
      HasDomEvent (DomEventType, domEvent),
      InputElement (..),
      InputElementConfig,
      KeyCode,
      MonadHold (holdDyn),
      PerformEvent (performEvent_),
      PostBuild (getPostBuild),
      Prerender (Client),
      Reflex
          ( Dynamic,
            Event,
            updated
          ),
      blank,
      delay,
      dyn_,
      el,
      el',
      elAttr,
      elAttr',
      elClass,
      elClass',
      elDynAttr,
      elDynClass,
      elDynClass',
      elementConfig_eventSpec,
      elementConfig_initialAttributes,
      elementConfig_modifyAttributes,
      fanMap,
      fmapMaybe,
      foldDyn,
      inputElementConfig_elementConfig,
      inputElementConfig_initialValue,
      inputElementConfig_setValue,
      leftmost,
      mergeWith,
      preventDefault,
      text,
      wrapDomEvent,
    )
import Shared
    (elLoginSignup, dynSimple,
      iFa,
      whenJust,
    )
import State
    (Session (..), Env (..),
      Navigation (..),
      State (..),
      stageUrl,
      updateState,
    )
import Text.Read (readMaybe)
import TextShow (TextShow (showt))
import qualified Palantype.Common.Dictionary.Numbers as Numbers
import Control.Monad (when)
import Data.Int (Int)
import Common.Model (AppState(..), Message(..))
import qualified Common.Stage as Stage
import Type.Reflection (eqTypeRep, (:~~:)(HRefl), typeRep)
import Control.Monad.IO.Class (MonadIO)
import Text.Show (Show(show))

default (Text)

elFileInput :: DomBuilder t m => Event t Text -> m (Event t File)
elFileInput eSet = do
    i <-
        inputElement $
            def
                & inputElementConfig_setValue
                .~ eSet
                & inputElementConfig_elementConfig
                    . elementConfig_initialAttributes
                .~ ("type" =: "file" <> "accept" =: "text/cfg")

    let eFiles = _inputElement_files i
    pure $ mapMaybe listToMaybe $ updated eFiles

message ::
    forall t (m :: * -> *).
    ( DomBuilder t m,
      PostBuild t m,
      MonadReader (Dynamic t State) m,
      EventWriter t (Endo State) m
    ) =>
    m ()
message = do
    dynMsg <- asks (fmap $ stApp >>> stMsg)
    dyn_ $
        dynMsg <&> \mMsg -> whenJust mMsg $ \Message {..} ->
            let spanClose = elClass' "span" "close" $ iFa "fas fa-times"
             in elClass "div" "msgOverlay" $ do
                    (elClose, _) <- spanClose
                    let eClose = domEvent Click elClose
                    updateState $ eClose $> [field @"stApp" . field @"stMsg" .~ Nothing]
                    el "div" $ text msgCaption
                    el "span" $ text msgBody

settings ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      MonadHold t m,
      Palantype key,
      PostBuild t m,
      Prerender t m,
      EventWriter t (Endo State) m,
      MonadReader (Dynamic t State) m,
      Routed t StageIndex m,
      SetRoute t (R FrontendRoute) m
    ) =>
    m ()
settings = elClass "div" "topmenu" do
    dynState <- ask
    let dynAppState = stApp <$> dynState
        lang = if
          | Just HRefl <- typeRep @key `eqTypeRep` typeRep @DE.Key -> DE
          | Just HRefl <- typeRep @key `eqTypeRep` typeRep @EN.Key -> EN
          | otherwise -> $failure "Key not implemented"

    elClass "div" "floatLeft" $ do
        -- button to show configuration dropdown
        eFile <- elClass "div" "topmenu-entry dropdown" $ do
            elClass "span" "icon-link big" $ iFa "fas fa-cog"
            elClass "div" "dropdown-content" $ mdo
                let dynMCfgName =
                        fmap pcfgName
                            . view (at lang >>> _Wrapped')
                            . stPloverCfg
                            <$> dynAppState
                    elCheckmark co =
                        dyn_ $ dynMCfgName <&> \cfgName ->
                            elClass "span" "checkmark" $
                                if cfgName == Just co
                                then text "✓ "
                                else blank

                elClass "span" "caption" $ text "Keyboard layout"

                (elQwertz, _) <- elClass' "span" "entry" $ do
                    elCheckmark CNQwertzDE
                    text "qwertz DE"

                let eQwertz = domEvent Click elQwertz
                updateState $
                    eQwertz
                        $> [ field @"stApp" . field @"stPloverCfg"
                                 . _Wrapped'
                                 . ix lang
                                 .~ keyMapToPloverCfg lsStenoQwertz [] "keyboard" CNQwertzDE
                           ]

                (elQwerty, _) <- elClass' "span" "entry" $ do
                    elCheckmark CNQwertyEN
                    text "qwerty EN"
                let eQwerty = domEvent Click elQwerty
                    lsStenoQwertyEN = case lang of
                        EN -> lsStenoQwertyOrig
                        _ -> lsStenoQwerty
                updateState $
                    eQwerty
                        $> [ field @"stApp" . field @"stPloverCfg"
                                 . _Wrapped'
                                 . ix lang
                                 .~ keyMapToPloverCfg lsStenoQwertyEN [] "keyboard" CNQwertyEN
                           ]

                eFile' <- elClass "span" "hiddenFileInput entry" $ do
                    elCheckmark CNFile
                    text "Upload plover.cfg"
                    elFileInput $ leftmost [eQwerty, eQwertz] $> ""

                elClass "span" "caption" $ text "Progress"

                (eRP, _) <- elClass' "span" "entry" $ text "Reset"
                updateState $ domEvent Click eRP $>
                    [ field @"stApp" . field @"stProgress" .~ def
                    ]

                dyn_ $ dynState <&> \State{..} -> case stSession of
                  SessionAnon -> blank
                  SessionUser _ -> do
                    dynStage <- askRoute
                    elClass "span" "caption" $ text "Account"
                    (domSettings, _) <- elClass' "span" "entry" $ text "Go to settings"
                    let evGotoSettings = tag (current dynStage) $ domEvent Click domSettings
                    updateState $ evGotoSettings <&> \stage ->
                      [ field @"stRedirectUrl" .~ (stageUrl @key) stage ]
                    setRoute $ evGotoSettings $> FrontendRoute_Auth :/ AuthPage_Settings :/ ()

                pure eFile'

        elClass "span" "vertical-line" blank

        -- button to switch between palantype systems, i.e. DE and EN

        elClass "div" "topmenu-entry dropdown" $ do
            elClass "span" "icon-link big" $ text $ showSymbol lang
            elClass "div" "dropdown-content" $ do
                (eRL, _) <- elClass' "span" "entry" $ text "Back to landing page"
                let eClickRL = domEvent Click eRL
                updateState $ eClickRL $> [ field @"stApp" . field @"stMLang" .~ Nothing]
                setRoute $ eClickRL $> FrontendRoute_Main :/ ()

        evText <- postRender $ do
            fileReader <- liftJSM newFileReader
            let encoding = Just ("utf8" :: String)
            performEvent_ $ eFile <&> \f -> readAsText fileReader (Just f) encoding
            fmap catMaybes $ wrapDomEvent fileReader (`on` load) $ liftJSM $ do
                v <- getResult fileReader
                (fromJSVal <=< toJSVal) v

        dynEitherText <- holdDyn (Left "no file") $ Right . Text.unpack <$> evText

        evRespConfigNew <- request $ postConfigNew dynEitherText $ void evText

        updateState $ mapMaybe (either Just (const Nothing)) evRespConfigNew <&>
            \str ->
                let msgCaption = "Error when loading file"
                    msgBody = "Did you upload a proper .cfg file?\n" <> str
                 in [ field @"stApp" . field @"stMsg" ?~ Message {..}
                    , field @"stApp" . field @"stPloverCfg" .~ defaultPloverCfg
                    ]

        updateState $ mapMaybe (either (const Nothing) Just) evRespConfigNew <&>
            \(ln, systemCfg@PloverSystemCfg {..}) ->
                [ field @"stApp" . field @"stPloverCfg" %~ (_Wrapped' %~ ix ln .~ systemCfg),
                  if null pcfgUnrecognizedQwertys
                      then id
                      else
                          let msgCaption = "Unrecognized qwerty keys"
                              msgBody =
                                  "Your key map contains unrecognized entries:\n"
                                      <> Text.intercalate "\n" pcfgUnrecognizedQwertys
                           in field @"stApp" . field @"stMsg" .~ Just Message {..},
                  if null pcfgUnrecognizedStenos
                      then id
                      else
                          let msgCaption = "Unrecognized steno keys"
                              msgBody =
                                  "Your key map contains unrecognized entries:\n"
                                      <> Text.intercalate
                                          "\n"
                                          (showt <$> pcfgUnrecognizedStenos)
                           in field @"stApp" . field @"stMsg" .~ Just Message {..}
                ]

        elClass "span" "vertical-line" blank

        -- button to toggle keyboard
        let dynClass = dynAppState <&> \st ->
                "topmenu-entry btnToggleKeyboard "
                    <> if stShowKeyboard st
                        then "keyboardVisible"
                        else "keyboardHidden"
        (s, _) <- elDynClass' "div" dynClass $ do
            iFa "fas fa-keyboard"
            el "code" $ text $ showt $ case lang of
              DE -> KI.toRaw @DE.Key kiInsert
              EN -> KI.toRaw @EN.Key kiInsert

        updateState $ domEvent Click s $> [field @"stApp" . field @"stShowKeyboard" %~ not]

    dynRedirectRoute <- fmap (stageUrl @key) <$> askRoute
    elLoginSignup dynRedirectRoute

    elClass "br" "clearBoth" blank

data KeyState key
    = KeyStateDown key
    | KeyStateUp key

{-# INLINEABLE keydown #-}
keydown ::
    ( Reflex t,
      HasDomEvent t e 'KeydownTag,
      DomEventType e 'KeydownTag ~ Word
    ) =>
    KeyCode ->
    e ->
    Event t ()
keydown keycode =
    fmapMaybe (\n -> guard $ fromIntegral n == keycode)
        . domEvent Keydown

{-# INLINEABLE keyup #-}
keyup ::
    ( Reflex t,
      HasDomEvent t e 'KeyupTag,
      DomEventType e 'KeyupTag ~ Word
    ) =>
    KeyCode ->
    e ->
    Event t ()
keyup keycode =
    fmapMaybe (\n -> guard $ fromIntegral n == keycode)
        . domEvent Keyup

data FanChord
    = FanToggle
    | FanDown
    | FanUp
    | FanPageUp
    | FanPageDown
    | FanOther
    deriving (Eq, Ord)

data StateInput key = StateInput
  {
    -- | the set of keys that are currently pressed
    stiKeysPressed :: Set key

  -- | the set of keys that have been pressed since the last
  --   release; a release is the event of no key pressed
  , stiKeysDown    :: Set key

  -- | a key chord, made from the set of keys that have been
  -- pressed since the last release
  -- the difference to `dynDownKeys`: dynChord changes
  -- state upon release, whereas dynDownKeys changes state
  -- every time a new key is pushed down AND upon release
  , stiMChord      :: Maybe (Chord key)
  }

stenoInput
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) (Client m)
       , MonadHold t m
       , MonadReader (Dynamic t State) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       )
    => m (Event t (Chord key))
stenoInput = do
    let lang = if
          | Just HRefl <- typeRep @key `eqTypeRep` typeRep @EN.Key -> EN
          | Just HRefl <- typeRep @key `eqTypeRep` typeRep @DE.Key -> DE
          | otherwise -> $failure "Key not implemented"
    dynPloverCfg <- asks $ fmap $ stApp >>> stPloverCfg
    dynKeyboardShowQwerty <- asks $ fmap $ stApp >>> stKeyboardShowQwerty
    dynShowKeyboard <- asks $ fmap $ stApp >>> stShowKeyboard
    let dynSystemCfg =
            view (_Wrapped' <<< at lang <<< non defaultPloverSystemCfg) <$> dynPloverCfg
    dynSimple $ zipDyn dynSystemCfg dynKeyboardShowQwerty <&>
        \(pcfg, showQwerty) -> postRender
                     $ elClass "div" "stenoInput"
                     $ stenoInput' lang pcfg showQwerty dynShowKeyboard
  where
    stenoInput' lang PloverSystemCfg{..} showQwerty dynShowKeyboard = mdo
        let keyChanges = pcfgLsKeySteno <&> \(qwertyKey, kI) ->
                [ keydown qwertyKey kbInput $> [KeyStateDown $ fromIndex kI]
                , keyup   qwertyKey kbInput $> [KeyStateUp   $ fromIndex kI]
                ]
            eKeyChange = mergeWith (<>) $ concat keyChanges

            register
              :: [KeyState key]
              -> StateInput key
              -> StateInput key
            register es StateInput{ stiKeysPressed, stiKeysDown } =
                let
                    stiKeysPressed' = foldl accDownUp stiKeysPressed es
                    (stiKeysDown', stiMChord) =
                        if Set.null stiKeysPressed'
                        then
                            ( Set.empty
                            , Just $ mkChord $ Set.elems stiKeysDown
                            )
                        else
                            ( foldl accDown stiKeysDown es
                            , Nothing
                            )

                in  StateInput
                      { stiKeysPressed = stiKeysPressed'
                      , stiKeysDown    = stiKeysDown'
                      , stiMChord
                      }
                 -- in (setKeys', word', release')
                where
                    accDownUp s (KeyStateDown k) = Set.insert k s
                    accDownUp s (KeyStateUp   k) = Set.delete k s
                    accDown   s (KeyStateDown k) = Set.insert k s
                    accDown   s (KeyStateUp   _) = s

        dynInput <-
            foldDyn
                register
                (StateInput Set.empty Set.empty Nothing)
                eKeyChange

        let
            dynClass = bool "displayNone" "" <$> dynShowKeyboard
        elDynClass "div" dynClass $ case lang of
            -- a bit of a hack to switch to the original Palantype
            -- keyboard layout for English
            -- that original layout I will consider the exception
            EN ->
                elKeyboardEN
                    pcfgName
                    pcfgMapStenoKeys
                    (stiKeysPressed <$> dynInput)
            _ ->
                elKeyboard
                    pcfgName
                    pcfgMapStenoKeys
                    (stiKeysPressed <$> dynInput)
                    lang
                    showQwerty

        kbInput <- elStenoOutput $ stiKeysDown <$> dynInput

        -- TODO: doesn't seem to have the desired effect
        let eLostFocus = filter not $ updated $ _inputElement_hasFocus kbInput
        performEvent_ $ eLostFocus $> focus (_inputElement_raw kbInput)

        -- post build auto focus: the post build event happens before the element
        -- is mounted. postmount event waits for pull request to be accepted
        -- https://github.com/reflex-frp/reflex-dom-semui/issues/18
        ePb <- delay 0.1 =<< getPostBuild
        performEvent_ $ ePb $> focus (_inputElement_raw kbInput)

        let eChordAll = catMaybes $ updated $ stiMChord <$> dynInput
            selector = fanMap $ eChordAll <&> \c -> if
                | fromChord c == KI.toRaw @key kiInsert -> Map.singleton FanToggle c
                | fromChord c == KI.toRaw @key kiUp     -> Map.singleton FanUp c
                | fromChord c == KI.toRaw @key kiDown   -> Map.singleton FanDown c
                | fromChord c == KI.toRaw @key kiPageUp -> Map.singleton FanPageUp c
                | fromChord c == KI.toRaw @key kiPageDown -> Map.singleton FanPageDown c
                | otherwise                             -> Map.singleton FanOther c
            eChordToggle   = select selector (Const2 FanToggle)
            eChordDown     = select selector (Const2 FanDown  )
            eChordUp       = select selector (Const2 FanUp    )
            eChordOther    = select selector (Const2 FanOther )
            eChordPageDown = select selector (Const2 FanPageDown  )
            eChordPageUp   = select selector (Const2 FanPageUp    )

        updateState $
            eChordToggle $> [field @"stApp" . field @"stShowKeyboard" %~ not]

        -- this is a workaround
        -- scroll, like focus, is not available in reflex dom
        -- GHCJS.DOM.Element.scroll relies on GhcjsDomSpace
        -- GhcjsDomSpace requires the elements to be build post render
        let jsScroll :: Int -> Text
            jsScroll x =
                "let el = document.getElementById(\"content\"); \
                \el.scrollBy(0," <> showt x <> ")"
        performEvent_ $ eChordDown     $> void (liftJSM $ eval $ jsScroll 100)
        performEvent_ $ eChordUp       $> void (liftJSM $ eval $ jsScroll (-100))
        performEvent_ $ eChordPageDown $> void (liftJSM $ eval $ jsScroll 600)
        performEvent_ $ eChordPageUp   $> void (liftJSM $ eval $ jsScroll (-600))

        pure eChordOther


elKeyboard
  :: forall key t (m :: * -> *)
  .  ( DomBuilder t m
     , EventWriter t (Endo State) m
     , Palantype key
     , PostBuild t m
     )
  => CfgName
  -> Map KeyIndex [Text]
  -> Dynamic t (Set key)
  -> Lang
  -> Bool
  -> m ()
elKeyboard cfgName stenoKeys dynPressedKeys lang showQwerty =
    elClass "div" "keyboard" $ do
        el "table" $ do
            el "tr" $ do
                elCell showQwerty stenoKeys dynPressedKeys 1 "1" "pinkyYOffset"
                elCell showQwerty stenoKeys dynPressedKeys 4 "1" ""
                elCell showQwerty stenoKeys dynPressedKeys 7 "1" ""
                elCell showQwerty stenoKeys dynPressedKeys 10 "1" ""
                elAttr "td" ("colspan" =: "2" <> "class" =: "gap") blank
                elAttr "td" ("colspan" =: "1" <> "class" =: "handgap") blank
                elAttr "td" ("colspan" =: "2" <> "class" =: "gap") blank
                elCell showQwerty stenoKeys dynPressedKeys 21 "1" ""
                elCell showQwerty stenoKeys dynPressedKeys 24 "1" ""
                elCell showQwerty stenoKeys dynPressedKeys 27 "1" ""
                elCell showQwerty stenoKeys dynPressedKeys 30 "1" "pinkyYOffset"
            el "tr" $ do
                elCell showQwerty stenoKeys dynPressedKeys 2 "1" "homerow pinkyYOffset"
                elCell showQwerty stenoKeys dynPressedKeys 5 "1" "homerow"
                elCell showQwerty stenoKeys dynPressedKeys 8 "1" "homerow"
                elCell showQwerty stenoKeys dynPressedKeys 11 "1" "homerow"
                elAttr "td" ("colspan" =: "2" <> "class" =: "gap") blank
                elAttr "td" ("colspan" =: "1" <> "class" =: "handgap") blank
                elAttr "td" ("colspan" =: "2" <> "class" =: "gap") blank
                elCell showQwerty stenoKeys dynPressedKeys 22 "1" "homerow"
                elCell showQwerty stenoKeys dynPressedKeys 25 "1" "homerow"
                elCell showQwerty stenoKeys dynPressedKeys 28 "1" "homerow"
                elCell showQwerty stenoKeys dynPressedKeys 31 "1" "homerow pinkyYOffset"
            el "tr" $ do
                elCell showQwerty stenoKeys dynPressedKeys 3 "1" "pinkyYOffset"
                elCell showQwerty stenoKeys dynPressedKeys 6 "1" ""
                elCell showQwerty stenoKeys dynPressedKeys 9 "1" ""
                elCell showQwerty stenoKeys dynPressedKeys 12 "1" ""
                elAttr "td" ("colspan" =: "2" <> "class" =: "gap") blank
                elAttr "td" ("colspan" =: "1" <> "class" =: "handgap") blank
                elAttr "td" ("colspan" =: "2" <> "class" =: "gap") blank
                elCell showQwerty stenoKeys dynPressedKeys 23 "1" ""
                elCell showQwerty stenoKeys dynPressedKeys 26 "1" ""
                elCell showQwerty stenoKeys dynPressedKeys 29 "1" ""
                elCell showQwerty stenoKeys dynPressedKeys 32 "1" "pinkyYOffset"
            el "tr" $ do
                elAttr "td" ("colspan" =: "2" <> "class" =: "gap") blank

                -- left thumb
                elCell showQwerty stenoKeys dynPressedKeys 13 "1" "thumbrow"
                elCell showQwerty stenoKeys dynPressedKeys 14 "1" "thumbrow"
                elCell showQwerty stenoKeys dynPressedKeys 15 "1" "homerow thumbrow"
                elCell showQwerty stenoKeys dynPressedKeys 16 "1" "thumbrow"

                elAttr "td" ("colspan" =: "1" <> "class" =: "handgap") blank

                -- right thumb
                elCell showQwerty stenoKeys dynPressedKeys 17 "1" "thumbrow"
                elCell showQwerty stenoKeys dynPressedKeys 18 "1" "homerow thumbrow"
                elCell showQwerty stenoKeys dynPressedKeys 19 "1" "thumbrow"
                elCell showQwerty stenoKeys dynPressedKeys 20 "1" "thumbrow"

                elAttr "td" ("colspan" =: "2" <> "class" =: "gap") blank
        elClass "div" "configuration" $ do
            el "div" $ text $ showt lang
            el "div" $ text $ showt cfgName

            ev <- _inputElement_checkedChange <$> inputElement
                ( def & inputElementConfig_initialChecked .~ showQwerty
                      & inputElementConfig_elementConfig
                          . elementConfig_initialAttributes
                              .~ (  "id"   =: "showQwerties"
                                 <> "type" =: "checkbox"
                                 )
                )
            updateState $ ev $> [field @"stApp" . field @"stKeyboardShowQwerty" %~ not]
            elAttr "label" ("for" =: "showQwerties") $ text "Show qwerty keys"

-- | original Palantype keyboard layout
-- | unfortunately the keys don't follow the simple order
-- | of top row, home row, bottom row
-- | therefore, I treat the original palantype layout as the exception
elKeyboardEN ::
    forall key t (m :: * -> *).
    (DomBuilder t m, Palantype key, PostBuild t m) =>
    CfgName ->
    Map KeyIndex [Text] ->
    Dynamic t (Set key) ->
    m ()
elKeyboardEN cfgName stenoKeys dynPressedKeys = elClass "div" "keyboard" $ do
    el "table" $ do
        el "tr" $ do
            elAttr "td" ("colspan" =: "1" <> "class" =: "gap") blank
            elCell True stenoKeys dynPressedKeys 4 "1" ""
            elCell True stenoKeys dynPressedKeys 7 "1" ""
            elCell True stenoKeys dynPressedKeys 10 "1" ""
            -- elAttr "td" ("colspan" =: "4" <> "class" =: "gap") blank
            elAttr "td" ("colspan" =: "1" <> "class" =: "gap") blank
            elAttr "td" ("colspan" =: "1" <> "class" =: "gap") blank
            elAttr "td" ("colspan" =: "1" <> "class" =: "gap") blank
            elAttr "td" ("colspan" =: "1" <> "class" =: "gap") blank
            elCell True stenoKeys dynPressedKeys 21 "1" ""
            elCell True stenoKeys dynPressedKeys 24 "1" ""
            elCell True stenoKeys dynPressedKeys 27 "1" ""
            elAttr "td" ("colspan" =: "1" <> "class" =: "gap") blank
        el "tr" $ do
            elCell True stenoKeys dynPressedKeys 1 "1" ""
            elCell True stenoKeys dynPressedKeys 5 "1" "homerow"
            elCell True stenoKeys dynPressedKeys 8 "1" "homerow"
            elCell True stenoKeys dynPressedKeys 11 "1" "homerow"
            elAttr "td" ("colspan" =: "4" <> "class" =: "gap") blank
            elCell True stenoKeys dynPressedKeys 22 "1" "homerow"
            elCell True stenoKeys dynPressedKeys 25 "1" "homerow"
            elCell True stenoKeys dynPressedKeys 28 "1" "homerow"
            elCell True stenoKeys dynPressedKeys 30 "1" ""
        el "tr" $ do
            elCell True stenoKeys dynPressedKeys 2 "1" "homerow"
            elCell True stenoKeys dynPressedKeys 6 "1" ""
            elCell True stenoKeys dynPressedKeys 9 "1" ""
            elCell True stenoKeys dynPressedKeys 12 "1" ""
            elAttr "td" ("colspan" =: "4" <> "class" =: "gap") blank
            elCell True stenoKeys dynPressedKeys 23 "1" ""
            elCell True stenoKeys dynPressedKeys 26 "1" ""
            elCell True stenoKeys dynPressedKeys 29 "1" ""
            elCell True stenoKeys dynPressedKeys 31 "1" "homerow"
        el "tr" $ do
            elCell True stenoKeys dynPressedKeys 3 "3" ""
            elCell True stenoKeys dynPressedKeys 14 "1" ""
            elCell True stenoKeys dynPressedKeys 15 "1" "homerow"
            elCell True stenoKeys dynPressedKeys 17 "2" ""
            elCell True stenoKeys dynPressedKeys 18 "1" "homerow"
            elCell True stenoKeys dynPressedKeys 19 "1" ""
            elCell True stenoKeys dynPressedKeys 32 "3" ""
    elClass "div" "configuration" $ do
        el "div" $ text $ showt EN
        el "div" $ text $ showt cfgName

elCell
  :: forall key t (m1 :: * -> *)
  . (DomBuilder t m1, Palantype key, PostBuild t m1)
  => Bool
  -> Map KeyIndex [Text]
  -> Dynamic t (Set key)
  -> KeyIndex
  -> Text
  -> Text
  -> m1 ()
elCell showQwerty stenoKeys dynPressedKeys i colspan strCls =
  -- -- | whether or not number input mode is active
  --, stiNumberMode  :: Bool
  -- -- check if the mode keys (DE: W, N) are being pressed
    case Map.lookup i stenoKeys of
        Nothing -> elAttr "td" ("colspan" =: colspan <> "class" =: "inactive") blank
        Just qwerties -> do
            let k = fromIndex i
                (strNumberMode, strNumberModeShift) = case Numbers.fromIndex i of
                    Nothing -> ("", "")
                    Just (str, mStrShift) -> (str, fromMaybe "" mStrShift)
                attrs = dynPressedKeys <&> \set' ->
                    let
                        isNumberMode = setModeKeys `Set.isSubsetOf` set'
                                     && not (fromIndex 2 `Set.member` set')
                        isNumberModeShift = setModeKeys `Set.isSubsetOf` set'
                                         && fromIndex 2 `Set.member` set'
                        noNumberMode = Text.null strNumberMode
                        noNumberModeShift = Text.null strNumberModeShift
                        lsClass = catMaybes
                            [ if k `Set.member` set' then Just "pressed" else Nothing
                            , if isNumberMode then Just "numberMode" else Nothing
                            , if isNumberModeShift then Just "numberModeShift" else Nothing
                            , if     keyCode k == '_'
                                  || (isNumberMode && noNumberMode)
                                  || (isNumberModeShift && noNumberModeShift)
                                  then Just "inactive"
                                  else Nothing
                            , if isNumberMode
                                then
                                    if Text.length strNumberMode >= 3
                                        then Just "verySmall"
                                        else if Text.length strNumberMode >= 2
                                            then Just "small"
                                            else Nothing
                                else Nothing
                            , if isNumberModeShift
                                then
                                    if Text.length strNumberModeShift >= 3
                                        then Just "verySmall"
                                        else if Text.length strNumberModeShift >= 2
                                            then Just "small"
                                            else Nothing
                                else Nothing
                            ]
                     in
                           "colspan" =: colspan
                        <> "class"   =: unwords (strCls : lsClass)
            elDynAttr "td" attrs $ do
                elClass "div" "steno" $ text $ Text.singleton $ keyCode k
                elClass "div" "numberMode" $ text strNumberMode
                elClass "div" "numberModeShift" $ text strNumberModeShift
                when showQwerty $ elClass "div" "qwerty" $ text $ Text.unwords qwerties
  where
    setModeKeys = Set.fromList $ fromIndex <$> [9, 11]

elStenoOutput
  :: forall key t (m :: * -> *)
  .  ( DomBuilder t m
     , MonadFix m
     , Palantype key
     )
  => Dynamic t (Set key)
  -> m (InputElement EventResult (DomBuilderSpace m) t)
elStenoOutput dynDownKeys = mdo
    let eFocus = updated (_inputElement_hasFocus i) <&> \case
            True -> ("Type!", "class" =: Just "anthrazit")
            False -> ("Click me!", "class" =: Just "red")
        eTyping = updated dynDownKeys <&> \downKeys ->
            if Set.null downKeys
                then ("...", "class" =: Nothing)
                else
                    ( showt $ mkChord $ Set.elems downKeys,
                      "class" =: Nothing
                    )
        eChange = leftmost [eFocus, eTyping]
        eSetValue = fst <$> eChange

    i <-
        inputElement $
            (def :: InputElementConfig EventResult t (DomBuilderSpace m))
                & inputElementConfig_setValue .~ eSetValue
                & inputElementConfig_elementConfig . elementConfig_modifyAttributes
                    .~ (snd <$> eChange)
                & inputElementConfig_initialValue .~ "Click me!"
                & inputElementConfig_elementConfig
                    . elementConfig_initialAttributes
                .~ ( "readonly"  =: "readonly"
                  <> "autofocus" =: "autofocus"
                  <> "class"     =: "red"
                  <> "id"        =: "stenoOutput"
                   )
                & inputElementConfig_elementConfig . elementConfig_eventSpec
                    %~ addEventSpecFlags
                        (Proxy :: Proxy (DomBuilderSpace m))
                        Keydown
                        (const preventDefault)
    pure i

-- Table of Contents

elTOC ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadReader (Dynamic t State) m,
      Palantype key,
      Prerender t m,
      PostBuild t m,
      RouteToUrl (R FrontendRoute) m,
      SetRoute t (R FrontendRoute) m
    ) =>
    StageIndex ->
    m ()
elTOC stageCurrent = elClass "section" "toc" $ do
    dynState <- asks $ fmap stApp
    let dynShowTOC = stShowTOC <$> dynState
        dynShowStage i = (Set.member i . stTOCShowStage) <$> dynState

    -- button to toggle TOC
    dyn_ $ dynShowTOC <&> \showTOC -> do
        (s, _) <- if showTOC
            then
                elAttr' "span"
                    (  "class" =: "btn TOCVisible"
                    <> "title" =: "Hide Table of Contents"
                    ) $ iFa "fas fa-times"
            else
                elAttr' "span"
                    (  "class" =: "btn TOCHidden"
                    <> "title" =: "Show Table of Contents"
                    ) $ iFa "fas fa-bars"

        updateState $ domEvent Click s $> [field @"stApp" . field @"stShowTOC" %~ not]

    let dynClassDisplay = bool "displayNone" "" <$> dynShowTOC
    elDynClass "div" dynClassDisplay $ do
        let dynCleared = stCleared <$> dynState
        dyn_ $ dynCleared <&> \cleared -> do

            let
                elLi iSubstage = do
                    let cls =
                            if iSubstage == stageCurrent
                                then "bgLightgray"
                                else ""
                    elClass "li" cls $ do
                        if iSubstage `Set.member` cleared
                            then elClass "span" "toc-checkmark" $ iFa "fas fa-check"
                            else el "span" $ text "○"
                        routeLink (stageUrl @key iSubstage) $ do
                            let (str1, mg, str2) = Stage.toTOCString $
                                  case Stage.fromIndex @key iSubstage of
                                    Nothing -> $failure $ "index invalid: " <> show iSubstage
                                    Just s  -> s
                            text str1
                            whenJust mg \g -> elClass "strong" "greediness" $ text $ "G" <> showt g <> " "
                            text str2

                elStage :: Int -> Text -> [StageIndex] -> m ()
                elStage i stageTitle iSubstages = do
                    (s, _) <- elClass' "li" "stage" $ do
                        let dynClass =
                                bool "fas fa-caret-right" "fas fa-caret-down"
                                    <$> dynShowStage i
                        elClass "span" "caret" $ elDynClass "i" dynClass blank
                        text $ "Stage " <> showt i <> ": " <> stageTitle

                        let dynClassUl = bool "displayNone" "" <$> dynShowStage i
                        elDynClass "ul" dynClassUl $ traverse_ elLi iSubstages

                    let eClickS = domEvent Click s
                    updateState $ eClickS $>
                        [ field @"stApp" . field @"stTOCShowStage" . at i %~
                            maybe (Just ()) (\_ -> Nothing)
                        ]

            el "ul" $ do

                elLi 0
                elStage 1 "The Palantype Alphabet"  [1 .. 8 ]
                let lang = if
                      | Just HRefl <- typeRep @key `eqTypeRep` typeRep @EN.Key -> EN
                      | Just HRefl <- typeRep @key `eqTypeRep` typeRep @DE.Key -> DE
                      | otherwise -> $failure "Key not implemented"
                case lang of
                  EN -> elStage 2 "Syllables and chords"           [9  .. 10]
                  DE -> do
                      elStage 2  "Syllables and chords"           [9  .. 12]
                      elStage 3  "Common replacement rules"       [13 .. 15]
                      elStage 4  "Be efficient, be greedy"        [16 .. 18]
                      elStage 5  "R, L, and Di consonants"         [19 .. 22]
                      elStage 6  "Stretching vowels with H and R" [23 .. 28]
                      elStage 7  "The dt-rule"                    [29 .. 30]
                      elStage 8  "Multiple vowels"                [31 .. 33]
                      elStage 9  "Replacing C and breaking IA/IO" [34 .. 37]
                      elStage 10 "Swapping s, sch, and z"         [38 .. 42]
                      elStage 11 "Double vowels and long a"       [43 .. 46]
                      elStage 12 "The silent h"                   [47 .. 48]
                      elStage 13 "Rare replacements and small s"  [49 .. 52]
                      elStage 14 "Briefs"                         [53]
                      elStage 15 "Real-life text input"           [54 .. 58]
                      elLi 59

landingPage
  :: forall t (m :: * -> *)
  . ( DomBuilder t m
    , EventWriter t (Endo State) m
    , MonadReader (Dynamic t State) m
    , SetRoute t (R FrontendRoute) m
    )
  => m ()
landingPage = elClass "div" "landing" $ do
    elClass "div" "title" $ el "h1" $ text "Palantype DE"
    elClass "div" "video-and-buttons" $ do
        elClass "div" "container" $ do
            elAttr "video"
                (  "width"    =: "480"
                <> "height"   =: "405"
                <> "autoplay" =: "autoplay"
                <> "muted"    =: "muted"
                <> "loop"     =: "loop"
                <> "preload"  =: "auto"
                <> "controls"  =: "controls"
                ) $ do
                elAttr "source" ("src" =: $(static "palantype-short.mp4") <> "type" =: "video/mp4") blank
                text "Your missing out on a great video here ¯\\_(ツ)_/¯"
            el "div" $ do
                el "h1" $ text "Type as fast as you speak"
                elClass "div" "right" $ do
                    (elDE, _) <- elClass "div" "action" $ do
                        el' "button" $ do
                            elClass "div" "container" $ do
                                elClass "div" "icon" $ do
                                    let elFlag cc =
                                          elClass "span" ("flag-icon flag-icon-squared flag-icon-" <> cc) blank
                                    elFlag "de"
                                    el "br" blank
                                    elFlag "at"
                                    elFlag "ch"
                                elClass "div" "countrycode" $ text "DE"
                                elClass "div" "description" $
                                    text
                                        "35 interactive tutorials. 2 Million words and growing. A steno system designed for \
                                        \the German language."
                            elClass "div" "cta" $ text "Start"

                    elEN <- elClass "div" "other" $ do
                        (elEN, _) <- el' "button" $
                            elClass "div" "container" $ do
                                elClass "div" "icon" $
                                    elAttr "img" ("src" =: $(static "palantype.png")) blank
                                elClass "div" "countrycode" $ text "EN"
                                elClass "div" "description" $
                                    text
                                        "The original palantype steno system for English, \
                                        \brought to your keyboard."

                        elClass "div" "button" $ el "div" $ do
                            text "Missing a language? Checkout the "
                            elAttr
                                "a"
                                ("href" =: "https://github.com/rubenmoor/palantype-tools")
                                $ text "source on Github"
                            text " to create your own Palantype-style steno system."
                        pure elEN

                    let evClick = leftmost
                          [ domEvent Click elEN $> EN
                          , domEvent Click elDE $> DE
                          ]
                    updateState $
                        evClick <&> \lang ->
                            [ field @"stApp" . field @"stMLang" ?~ lang,
                              -- if no progress in map, insert "Introduction"
                              field @"stApp" . field @"stProgress"
                                  %~ Map.insertWith (\_ o -> o) lang ($readLoc "introduction")
                            ]
                    dynState <- ask
                    let toMNewRoute st _ = do
                          lang <- st ^. field @"stApp" . field @"stMLang"
                          stage <- st ^. field @"stApp" . field @"stProgress" . at lang
                          pure $ case lang of
                            DE -> stageUrl @DE.Key stage
                            EN -> stageUrl @EN.Key stage
                    setRoute $ attachPromptlyDynWithMaybe toMNewRoute dynState evClick

    elClass "div" "info-columns" $ do

        elClass "div" "usp" $ do
            elClass "div" "icon" $ iFa "fas fa-rocket"
            elClass "div" "caption" $ text "Maximum typing speed"
            elClass "div" "description" $
                text
                    "Reach a typing speed of up to 300 \
                    \words per minute, fast enough to type along as people talk."

        elClass "div" "usp" $ do
            elClass "div" "icon" $ elAttr "img"
                 (  "src" =: $(static "chords.gif")
                 <> "width" =: "128"
                 <> "height" =: "128"
                 ) blank
            elClass "div" "caption" $ text "Type chords, not letters"
            elClass "div" "description" $
                text
                    "Input whole words or word parts with a single stroke \
                    \using multiple fingers at once. This is why it's so fast \
                    \and why it requires a lot of practice."

        elClass "div" "usp" $ do
            elClass "div" "icon" $ elAttr "img"
                 (  "src" =: $(static "keyboard-icon.png")
                 <> "width" =: "128"
                 <> "height" =: "128"
                 ) blank
            elClass "div" "caption" $ text "No special hardware"
            elClass "div" "description" $ do
                text "You will need a keyboard that supports "
                elAttr
                    "a"
                    ( "href"
                          =: "https://en.wikipedia.org/wiki/Rollover_(keyboard)"
                    )
                    $ text "N-key roll-over"
                text
                    ", to register all the keys that you press simultaneously, and \
                    \optionally an ortho-linear key layout."

        elClass "div" "usp" $ do
            elClass "div" "icon" $ elAttr "img"
                 (  "src" =: $(static "opensource-icon.png")
                 <> "width" =: "100"
                 <> "height" =: "100"
                 ) blank
            elClass "div" "caption" $ text "Free and open-source"
            elClass "div" "description" $ do
                text
                    "Find the code on Github and contribute by reporting bugs \
                    \and requesting features in the "
                elAttr
                    "a"
                    ( "href"
                          =: "https://github.com/rubenmoor/learn-palantype/issues"
                    )
                    $ text "issue tracker"
                text "."

    elClass "div" "reach-out" $ do
        text "Want to reach out? Join the "
        elAttr "a" ("href" =: "https://discord.gg/spymr5aCr5") $
            text "Plover Discord Server"
        text " and find me in #palantype, @gurubm."

    elClass "div" "tech-stack" $ do
      el "h1" $ text "The technology"

      el "dl" $ do
        el "dt" $ text "Obsidian Systems Obelisk"
        el "dd" $ do
          text "Functional reactive programming for web and mobile—a sublime experience, find the "
          elAttr "a" ("href" =: "https://github.com/obsidiansystems/obelisk") $ text "code on GitHub"
          text "."
        el "dt" $ text "GHCJS"
        el "dd" $ do
          text "Let the JavaScript be generated and stay type-safe and functional all the way, cf. "
          elAttr "a" ("href" =: "https://github.com/ghcjs/ghcjs") $ text "GHCJS on GitHub"
          text "."
        el "dt" $ text "Haskell"
        el "dd" $ do
          text "Category theory, lazy evaluation, purely functional programming since 1990. "
          elAttr "a" ("href" =: "https://en.wikipedia.org/wiki/Haskell") $ text "Read more on Wikipedia"
          text "."


elStages
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , MonadHold t m
       , MonadIO (Performable m)
       , MonadFix m
       , Palantype key
       , PerformEvent t m
       , PostBuild t m
       , Prerender t m
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       , TriggerEvent t m
       )
    => Lang
    -> (Event t () -> Event t ())
    -> RoutedT t StageIndex (ReaderT (Dynamic t State) (EventWriterT t (Endo State) m)) ()
elStages navLang toReady = do
    el "header" $ settings @key
    dynCurrent <- askRoute
    dyn_ $ dynCurrent <&> stages'
  where
    stages' ::
        StageIndex ->
        RoutedT t StageIndex (ReaderT (Dynamic t State) (EventWriterT t (Endo State) m)) ()
    stages' iCurrent = elClass "div" "box" $ do

        eChord <- stenoInput @key

        navigation <- elClass "div" "row" $ mdo
            elTOC @key iCurrent

            let setEnv page =
                    let navMPrevious = mPrev iCurrent
                        navCurrent = iCurrent
                        navMNext = mNext @key iCurrent
                     in mapRoutedT
                            ( withReaderT $ \dynState ->
                                  Env
                                      { envDynState = dynState
                                      , envEChord = eChord
                                      , envNavigation = Navigation {..}
                                      , envToReady = toReady
                                      }
                            ) do
                              dynRoute <- askRoute
                              evReady <- toReady <$> getPostBuild
                              Env{..} <- ask
                              void $ request $ postEventViewPage
                                (getMaybeAuthData <$> envDynState)
                                (Right . showRoute . stageUrl @key <$> dynRoute)
                                evReady
                              page
            navigation <- elAttr "section" ("id" =: "content") $ do
                elClass "div" "scrollTop" $ text
                    $  "▲ "
                    <> showt (KI.toRaw @key kiUp)
                    <> "  ↟ " <> showt (KI.toRaw @key kiPageUp)
                nav <- elClass "div" "content" $ setEnv $ do
                  let
                      navigationNothing = Navigation navLang Nothing 0 Nothing
                      elPageNotImplemented str = do
                        elClass "div" "small anthrazit" $
                          text ("Page not implemented: StageIndex " <> showt iCurrent)
                        elClass "div" "small anthrazit" $
                          text str

                  case Stage.fromIndex iCurrent of
                    Just (Stage (StageSpecial str) _) -> case str of
                      "Introduction"              -> introduction
                      "Type the letters"          -> Stage1.exercise1
                      "Memorize the order"        -> Stage1.exercise2
                      "Type the letters blindly"  -> Stage1.exercise3
                      "Memorize the order blindly"-> Stage1.exercise4
                      "Memorize the left hand"    -> Stage1.exercise5
                      "Memorize the right hand"   -> Stage1.exercise6
                      "Memorize home row"         -> Stage1.exercise7
                      "Memorize them all"         -> Stage1.exercise8
                      "Building muscle memory"    -> Stage2.exercise1
                      "Learn your first chords"   -> Stage2.exercise2
                      "Onset, nucleus, and coda"  -> Stage2.exercise3
                      "Syllabes and word parts"   -> Stage2.exercise4
                      "Plover Commands"           -> ploverCommands
                      "Fingerspelling"            -> fingerspelling
                      "Number Mode"               -> numberMode
                      "Command Keys"              -> commandKeys
                      "Special Characters"        -> specialCharacters
                      "Pattern Overview"          -> Patterns.overview
                      _                           ->
                        elPageNotImplemented "special page not found" $> navigationNothing
                    Just (Stage (StageGeneric pg g) h) ->
                      getGenericExercise (pg, g) h $
                        elPageNotImplemented "generic exercise not found" $> navigationNothing
                    Nothing -> elPageNotImplemented "index invalid" $> navigationNothing

                elClass "div" "scrollBottom"
                    $ text
                    $ "▼ "
                    <> showt (KI.toRaw @key kiDown)
                    <> "  ↡ "
                    <> showt (KI.toRaw @key kiPageDown)
                pure nav
            pure navigation
        elFooter @key navigation
