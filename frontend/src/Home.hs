{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Home where

import           Client                      (reqFailure, postConfigNew, postRender)
import           Common.Api                  (PloverCfg (..))
import           Common.Route                (FrontendRoute (..))
import           Control.Applicative         (Applicative (..))
import           Control.Category            (Category(id, (.)))
import           Control.Lens.Setter         ((%~), (.~))
import           Control.Monad               ((<=<), (=<<))
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.Reader        (MonadReader (ask), asks)
import           Data.Bool                   (Bool (..), bool, not)
import           Data.Default                (Default (def))
import           Data.Either                 (Either (..))
import           Data.Eq                     (Eq ((==)))
import           Data.Foldable               (Foldable (foldl, null), concat)
import           Data.Function               (const, ($), (&))
import           Data.Functor                (fmap, void, ($>), (<$>), (<&>))
import           Data.Generics.Product       (field)
import           Data.List                   (elem)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (Maybe (..), listToMaybe)
import           Data.Monoid                 (Monoid (mempty), (<>))
import           Data.Proxy                  (Proxy (..))
import           Data.Semigroup              (Endo (..))
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.String                 (String)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Tuple                  (fst, snd)
import           Data.Witherable             (Filterable (catMaybes, filter, mapMaybe))
import           GHCJS.DOM.EventM            (on)
import           GHCJS.DOM.FileReader        (getResult, load, newFileReader,
                                              readAsText)
import           GHCJS.DOM.HTMLElement       (focus)
import           GHCJS.DOM.Types             (File)
import           Language.Javascript.JSaddle (FromJSVal (fromJSVal),
                                              ToJSVal (toJSVal), liftJSM)
import           Obelisk.Route.Frontend      (pattern (:/), dynRouteLink, R, RouteToUrl, SetRoute,
                                              routeLink)
import           Reflex.Dom                  (DomBuilder (DomBuilderSpace, inputElement),
                                              DomSpace (addEventSpecFlags),
                                              EventName (Click, Keydown),
                                              EventResult, EventWriter,
                                              HasDomEvent (domEvent),
                                              InputElement (..),
                                              InputElementConfig,
                                              MonadHold (holdDyn),
                                              PerformEvent (performEvent_),
                                              PostBuild (getPostBuild),
                                              Prerender (Client),
                                              Reflex (Dynamic, Event, updated),
                                              blank, delay, dyn_, el, el',
                                              elAttr, elAttr', elClass,
                                              elClass', elDynAttr, elDynClass,
                                              elementConfig_eventSpec,
                                              elementConfig_initialAttributes,
                                              elementConfig_modifyAttributes,
                                              foldDyn,
                                              inputElementConfig_elementConfig,
                                              inputElementConfig_initialValue,
                                              inputElementConfig_setValue,
                                              keydown, keyup, leftmost,
                                              mergeWith, preventDefault, text, wrapDomEvent,
                                              (=:))
import           Servant.Common.Req          (reqSuccess)
import           Shared                      (dynSimple, iFa, prerenderSimple,
                                              whenJust)
import           State                       (Lang, Message (..),
                                              Stage (..), State (..),
                                              stageDescription, stageUrl,
                                              updateState)
import Palantype.Common.RawSteno (parseChordLenient, RawSteno(..))
import Palantype.Common (mkChord, Chord (..), Palantype(fromIndex), KeyIndex(..))
import TextShow (TextShow(showt))
import qualified Palantype.EN.Keys as EN
import Data.Proxied (dataTypeOfProxied)
import Data.Typeable (typeRep)

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
  , EventWriter t (Endo State) m
  )
  => m ()
message = do
  dynMsg <- asks (stMsg <$>)
  dyn_ $ dynMsg <&> \mMsg -> whenJust mMsg $ \Message {..} ->
    let spanClose = elClass' "span" "close" $ iFa "fas fa-times"
    in  elClass "div" "msgOverlay" $ do
          (elClose, _) <- spanClose
          let eClose = domEvent Click elClose
          updateState $ eClose $> [field @"stMsg" .~ Nothing]
          el "div" $ text msgCaption
          el "span" $ text msgBody

settings
  :: forall t js (m :: * -> *).
  ( DomBuilder t m
  , PostBuild t m
  , Prerender js t m
  , EventWriter t (Endo State) m
  , MonadReader (Dynamic t State) m
  , MonadFix m
  )
  => m ()
settings = do
  dynState <- ask


  -- button to toggle keyboard
  let dynShowKeyboard = stShowKeyboard <$> dynState
  dyn_ $ dynShowKeyboard <&> \showKeyboard -> do
    (s, _) <-
      if showKeyboard
        then
          elClass' "span" "btnHeader keyboardVisible" $
            iFa "far fa-keyboard"
        else
          elClass' "span" "btnHeader keyboardHidden" $
            iFa "fas fa-keyboard"

    updateState $ domEvent Click s $> [field @"stShowKeyboard" %~ not]

  -- button to show configuration dropdown
  eFile <- elClass "div" "dropdown" $ do
    elClass "span" "dropdown-button" $ iFa "fas fa-cog"
    elClass "div" "dropdown-content" $ mdo
      eFile' <- elClass "span" "hiddenFileInput" $ do
        text "Upload your plover.cfg"
        elFileInput $ eReset $> ""
      eReset <- do
        (spanResetConfig, _) <- el' "span" $ text "Reset to default configuration"
        let eReset' = domEvent Click spanResetConfig
        updateState $ eReset $> [field @"stPloverCfg" .~ def]
        pure eReset'

      (e, _) <- el' "span" $ text "Reset progress"
      updateState $ domEvent Click e $> [field @"stProgress" .~ def]

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
       in [ field @"stMsg" .~ Just Message {..}
          , field @"stPloverCfg" .~ def
          ]

  let compatibleSystems =
        ["Palantype", "Possum Palantype", "Possum Palantype German"]
      isCompatible system = system `elem` compatibleSystems

  updateState $ eReqSuccess <&> \ploverCfg@PloverCfg {..} ->
      [ field @"stPloverCfg" .~ ploverCfg
      , if null pcfgUnrecognizedQwertys then id
        else let msgCaption = "Unrecognized qwerty keys"
                 msgBody =
                   "Your key map contains unrecognized entries:\n"
                     <> Text.intercalate "\n" pcfgUnrecognizedQwertys
             in field @"stMsg" .~ Just Message {..}
      , if null pcfgUnrecognizedStenos then id
        else let msgCaption = "Unrecognized steno keys"
                 msgBody =
                      "Your key map contains unrecognized entries:\n"
                   <> Text.intercalate "\n"
                        (unRawSteno <$> pcfgUnrecognizedStenos)
             in field @"stMsg" .~ Just Message {..}
      , if isCompatible pcfgSystem then id
        else let msgCaption = "Incompatible system"
                 msgBody =
                      "Your system is " <> pcfgSystem
                   <> "\nCompatible systems at the moment are\n"
                   <> Text.intercalate "\n" compatibleSystems
             in field @"stMsg" .~ Just Message {..}
      ]

data KeyState key
  = KeyStateDown key
  | KeyStateUp key

stenoInput
  :: forall js (proxy :: * -> *) key t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t (Endo State) (Client m)
  , MonadHold t m
  , MonadReader (Dynamic t State) m
  , Palantype key
  , PostBuild t m
  , Prerender js t m
  )
  => proxy key
  -> m (Event t (Chord key))
stenoInput p = do
  dynPloverCfg <- asks (stPloverCfg <$>)
  dynShowKeyboard <- asks (stShowKeyboard <$>)
  dynSimple $ dynPloverCfg <&> \PloverCfg {..} -> do
    prerenderSimple $ elClass "div" "stenoInput" $ mdo
      let keyChanges =
            pcfgLsKeySteno <&> \(qwertyKey, kI) ->
              [ keydown qwertyKey kbInput $> [KeyStateDown $ fromIndex kI]
              , keyup   qwertyKey kbInput $> [KeyStateUp   $ fromIndex kI]
              ]

          eKeyChange = mergeWith (<>) $ concat keyChanges

          register
            :: [KeyState key]
            -> (Set key, Set key, Maybe (Chord key))
            -> (Set key, Set key, Maybe (Chord key))
          register es (keys, word, _) =
            let setKeys' = foldl accDownUp keys es
                (word', release') =
                  if Set.null setKeys'
                    then (Set.empty, Just $ mkChord $ Set.elems word)
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
      let dynPressedKeys = dynInput <&> \(keys, _   , _      ) -> keys
          dynDownKeys    = dynInput <&> \(_   , down, _      ) -> down
          dynChord       = dynInput <&> \(_   , _   , release) -> release

      let dynClass = bool "displayNone" "" <$> dynShowKeyboard
      elDynClass "div" dynClass $
        -- a bit of a hack to switch to the original Palantype keyboard layout
        -- for English
        -- that original layout I will consider the exception
        if typeRep p == typeRep (Proxy :: Proxy EN.Key)
           then elPTKeyboardEN pcfgMapStenoKeys dynPressedKeys pcfgSystem
           else elPTKeyboard pcfgMapStenoKeys dynPressedKeys pcfgSystem

      kbInput <- elStenoOutput dynDownKeys

      -- TODO: doesn't seem to have the desired effect
      let eLostFocus = filter not $ updated $ _inputElement_hasFocus kbInput
      performEvent_ $ eLostFocus $> focus (_inputElement_raw kbInput)

      -- post build auto focus: the post build event happens before the element
      -- is mounted. postmount event waits for pull request to be accepted
      -- https://github.com/reflex-frp/reflex-dom-semui/issues/18
      ePb <- delay 0.1 =<< getPostBuild
      performEvent_ $ ePb $> focus (_inputElement_raw kbInput)

      let eChord = catMaybes $ updated dynChord
          eChordSTFL = filter (== parseChordLenient "STFL") eChord
      updateState $ eChordSTFL $> [field @"stShowKeyboard" %~ not]

      pure eChord

elPTKeyboard
  :: forall key t (m :: * -> *).
  ( DomBuilder t m
  , Palantype key
  , PostBuild t m
  )
  => Map KeyIndex [Text]
  -> Dynamic t (Set key)
  -> Text
  -> m ()
elPTKeyboard stenoKeys dynPressedKeys system =
  elClass "div" "keyboard" $ do
    el "table" $ do
      el "tr" $ do
        elAttr "td" ("colspan" =: "1" <> "class" =: "gap") blank
        elCell stenoKeys dynPressedKeys 4 "1" False
        elCell stenoKeys dynPressedKeys 7 "1" False
        elCell stenoKeys dynPressedKeys 10 "1" False
        elAttr "td" ("colspan" =: "4" <> "class" =: "gap") blank
        elCell stenoKeys dynPressedKeys 21 "1" False
        elCell stenoKeys dynPressedKeys 24 "1" False
        elCell stenoKeys dynPressedKeys 27 "1" False
        elAttr "td" ("colspan" =: "1" <> "class" =: "gap") blank
      el "tr" $ do
        elCell stenoKeys dynPressedKeys 1 "1" False
        elCell stenoKeys dynPressedKeys 5 "1" True
        elCell stenoKeys dynPressedKeys 8 "1" True
        elCell stenoKeys dynPressedKeys 11 "1" True
        elAttr "td" ("colspan" =: "4" <> "class" =: "gap") blank
        elCell stenoKeys dynPressedKeys 22 "1" True
        elCell stenoKeys dynPressedKeys 25 "1" True
        elCell stenoKeys dynPressedKeys 28 "1" True
        elCell stenoKeys dynPressedKeys 30 "1" False
      el "tr" $ do
        elCell stenoKeys dynPressedKeys 2 "1" True
        elCell stenoKeys dynPressedKeys 6 "1" False
        elCell stenoKeys dynPressedKeys 9 "1" False
        elCell stenoKeys dynPressedKeys 12 "1" False
        elAttr "td" ("colspan" =: "4" <> "class" =: "gap") blank
        elCell stenoKeys dynPressedKeys 23 "1" False
        elCell stenoKeys dynPressedKeys 26 "1" False
        elCell stenoKeys dynPressedKeys 29 "1" False
        elCell stenoKeys dynPressedKeys 31 "1" True
      el "tr" $ do
        elCell stenoKeys dynPressedKeys 3 "2" False

        -- left thumb
        elCell stenoKeys dynPressedKeys 13 "1" False
        elCell stenoKeys dynPressedKeys 14 "1" False
        elCell stenoKeys dynPressedKeys 15 "1" True
        elCell stenoKeys dynPressedKeys 16 "1" True

        -- right thumb
        elCell stenoKeys dynPressedKeys 17 "1" False
        elCell stenoKeys dynPressedKeys 18 "1" False
        elCell stenoKeys dynPressedKeys 19 "1" True
        elCell stenoKeys dynPressedKeys 20 "1" False

        elCell stenoKeys dynPressedKeys 21 "2" False
    elClass "span" "system" $ text system

-- | original Palantype keyboard layout
-- | unfortunately the keys don't follow the simple order
-- | of top row, home row, bottom row
elPTKeyboardEN
  :: forall key t (m :: * -> *).
  ( DomBuilder t m
  , Palantype key
  , PostBuild t m
  )
  => Map KeyIndex [Text]
  -> Dynamic t (Set key)
  -> Text
  -> m ()
elPTKeyboardEN stenoKeys dynPressedKeys system =
  elClass "div" "keyboard" $ do
    el "table" $ do
      el "tr" $ do
        elAttr "td" ("colspan" =: "1" <> "class" =: "gap") blank
        elCell stenoKeys dynPressedKeys 3 "1" False
        elCell stenoKeys dynPressedKeys 7 "1" False
        elCell stenoKeys dynPressedKeys 10 "1" False
        elAttr "td" ("colspan" =: "4" <> "class" =: "gap") blank
        elCell stenoKeys dynPressedKeys 22 "1" False
        elCell stenoKeys dynPressedKeys 25 "1" False
        elCell stenoKeys dynPressedKeys 28 "1" False
        elAttr "td" ("colspan" =: "1" <> "class" =: "gap") blank
      el "tr" $ do
        elCell stenoKeys dynPressedKeys 2 "1" False
        elCell stenoKeys dynPressedKeys 4 "1" True
        elCell stenoKeys dynPressedKeys 8 "1" True
        elCell stenoKeys dynPressedKeys 11 "1" True
        elAttr "td" ("colspan" =: "4" <> "class" =: "gap") blank
        elCell stenoKeys dynPressedKeys 23 "1" True
        elCell stenoKeys dynPressedKeys 26 "1" True
        elCell stenoKeys dynPressedKeys 29 "1" True
        elCell stenoKeys dynPressedKeys 32 "1" False
      el "tr" $ do
        elCell stenoKeys dynPressedKeys 1 "1" True
        elCell stenoKeys dynPressedKeys 5 "1" False
        elCell stenoKeys dynPressedKeys 9 "1" False
        elCell stenoKeys dynPressedKeys 12 "1" False
        elAttr "td" ("colspan" =: "4" <> "class" =: "gap") blank
        elCell stenoKeys dynPressedKeys 24 "1" False
        elCell stenoKeys dynPressedKeys 27 "1" False
        elCell stenoKeys dynPressedKeys 30 "1" False
        elCell stenoKeys dynPressedKeys 31 "1" True
      el "tr" $ do
        elCell stenoKeys dynPressedKeys 6 "2" False
        -- 13: not in use
        elAttr "td" ("colspan" =: "1" <> "class" =: "gap") blank
        elCell stenoKeys dynPressedKeys 14 "1" False
        elCell stenoKeys dynPressedKeys 15 "1" True
        -- 16: not in use
        elAttr "td" ("colspan" =: "1" <> "class" =: "gap") blank
        elCell stenoKeys dynPressedKeys 18 "1" False
        elCell stenoKeys dynPressedKeys 19 "1" True
        elCell stenoKeys dynPressedKeys 20 "1" False
        elCell stenoKeys dynPressedKeys 21 "2" False
    elClass "span" "system" $ text system

elCell
  :: forall key t (m1 :: * -> *).
  ( DomBuilder t m1
  , Palantype key
  , PostBuild t m1
  )
  => Map KeyIndex [Text]
  -> Dynamic t (Set key)
  -> KeyIndex
  -> Text
  -> Bool
  -> m1 ()
elCell stenoKeys dynPressedKeys i colspan isHomerow =
  let mQwertyKeys = Map.lookup i stenoKeys

      showQwerties Nothing   = ""
      showQwerties (Just ks) = Text.unwords ks

      attrs =
        dynPressedKeys <&> \set' ->
          "colspan" =: colspan
            <> case (Set.member (fromIndex i) set', isHomerow) of
                 (True , True ) -> "class" =: "pressed homerow"
                 (True , False) -> "class" =: "pressed"
                 (False, True ) -> "class" =: "homerow"
                 (False, False) -> mempty
   in if Map.member i stenoKeys
        then elDynAttr "td" attrs $ do
          elClass "div" "steno " $ text $ showt $ (fromIndex :: KeyIndex -> key) i
          elClass "div" "qwerty " $ text $ showQwerties mQwertyKeys
        else elAttr "td" ("colspan" =: colspan <> "class" =: "gap") blank

elStenoOutput
  :: forall key t (m :: * -> *).
  ( DomBuilder t m
  , MonadFix m
  , Palantype key
  )
  => Dynamic t (Set key)
  -> m (InputElement EventResult (DomBuilderSpace m) t)
elStenoOutput dynDownKeys = mdo
  let eFocus =
        updated (_inputElement_hasFocus i) <&> \case
          True -> ("Type!", "class" =: Just "anthrazit")
          False -> ("Click me!", "class" =: Just "red")
      eTyping =
        updated dynDownKeys <&> \pressedKeys ->
          if Set.null pressedKeys
            then ("...",                                   "class" =: Nothing)
            else (showt $ mkChord $ Set.elems pressedKeys, "class" =: Nothing)
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

-- Table of Contents

toc
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t (Endo State) m
  , MonadReader (Dynamic t State) m
  , Prerender js t m
  , PostBuild t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  )
  => Lang
  -> Dynamic t Stage
  -> m ()
toc lang dynCurrent = elClass "section" "toc" $ do

  dynState <- ask
  let dynShowTOC = stShowTOC <$> dynState
      dynShowStage1 = stTOCShowStage1 <$> dynState
      dynShowStage2 = stTOCShowStage2 <$> dynState
      dynShowStage3 = stTOCShowStage3 <$> dynState
  -- button to toggle TOC
  dyn_ $ dynShowTOC <&> \showTOC -> do
    (s, _) <-
      if showTOC
        then
          elAttr' "span"
            (  "class" =: "btn TOCVisible"
            <> "title" =: "Hide Table of Contents"
            )
            $ iFa "fas fa-times"
        else
          elAttr' "span"
            (  "class" =: "btn TOCHidden"
            <> "title" =: "Show Table of Contents"
            )
            $ iFa "fas fa-bars"

    updateState $ domEvent Click s $> [field @"stShowTOC" %~ not]

  let dynClassDisplay = bool "displayNone" "" <$> dynShowTOC
  elDynClass "div" dynClassDisplay $ do
    let dynCleared = stCleared <$> dynState
    dyn_ $ dynCleared <&> \cleared -> do

      let elLi stage = do
            let dynClass = bool "" "bgLightgray" . (== stage) <$> dynCurrent
            elDynClass "li" dynClass $ do
              if stage `Set.member` cleared
                then iFa "fas fa-check"
                else el "span" $ text "○"
              routeLink (stageUrl lang stage) $ text $ stageDescription stage

      el "ul" $ do

        elLi Introduction

        (s1, _) <- elClass' "li" "stage" $ do

          let dynClass =
                bool "fas fa-caret-right" "fas fa-caret-down" <$> dynShowStage1
          elDynClass "i" dynClass blank
          text "Stage 1: The Palantype Alphabet"

        let eClickS1 = domEvent Click s1
        updateState $ eClickS1 $> [field @"stTOCShowStage1" %~ not]

        let dynClassUl1 =
              bool "displayNone" "" <$> dynShowStage1

        elDynClass "ul" dynClassUl1 $ do

          elLi Stage1_1
          elLi Stage1_2
          elLi Stage1_3
          elLi Stage1_4
          elLi Stage1_5
          elLi Stage1_6
          elLi Stage1_7

        (s2, _) <- elClass' "li" "stage" $ do

          let dynClass =
                bool "fas fa-caret-right" "fas fa-caret-down" <$> dynShowStage2
          elDynClass "i" dynClass blank
          text "Stage 2: Syllables and chords"

        let eClickS2 = domEvent Click s2
        updateState $ eClickS2 $> [field @"stTOCShowStage2" %~ not]

        let dynClassUl2 =
              bool "displayNone" "" <$> dynShowStage2

        elDynClass "ul" dynClassUl2 $ do
          elLi Stage2_1
          elLi Stage2_2
