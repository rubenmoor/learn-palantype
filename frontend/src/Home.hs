{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Home where

import           Client                      (postConfigNew, postRender)
import           Common.Alphabet             (PTChar (..), showKey)
import           Common.Api                  (PloverCfg (..))
import           Common.Keys                 (fromPlover)
import           Common.Route                (FrontendRoute (FrontendRoute_Main))
import           Control.Applicative         (Applicative (..))
import           Control.Category            (Category (id, (.)))
import           Control.Lens.Setter         (set, (%~), (.~))
import           Control.Monad               (unless, when, (<=<))
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.Reader        (MonadReader (ask), asks)
import           Data.Bool                   (Bool (..), not, (&&))
import           Data.Default                (Default (def))
import           Data.Either                 (Either (..))
import           Data.Eq                     (Eq ((/=)))
import           Data.Foldable               (Foldable (foldl, null), concat,
                                              for_)
import           Data.Function               (($), (&))
import           Data.Functor                (fmap, void, ($>), (<$>), (<&>))
import           Data.Generics.Product       (field)
import           Data.List                   (elem, sort, zip)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (Maybe (..), isNothing,
                                              listToMaybe)
import           Data.Monoid                 (Monoid(mconcat, mempty), (<>))
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.String                 (String, unwords)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Traversable            (Traversable (sequenceA))
import           Data.Tuple                  (fst)
import           Data.Witherable             (Filterable (catMaybes, mapMaybe))
import           GHCJS.DOM.FileReader        (getResult, load, newFileReader,
                                              readAsText)
import           JSDOM.EventM                (on)
import           JSDOM.Types                 (File)
import           Language.Javascript.JSaddle (FromJSVal (fromJSVal),
                                              ToJSVal (toJSVal), liftJSM)
import           Obelisk.Route.Frontend      (pattern (:/), R,
                                              SetRoute (setRoute))
import           Reflex.Dom                  (DomBuilder (DomBuilderSpace, inputElement),
                                              Element, EventName (Click),
                                              EventResult, EventTag (ClickTag),
                                              EventWriter,
                                              HasDomEvent (DomEventType, domEvent),
                                              InputElement (..),
                                              MonadHold (holdDyn),
                                              PerformEvent (performEvent_),
                                              PostBuild, Prerender,
                                              Reflex (Dynamic, Event, updated),
                                              blank, dynText, dyn_, el, el',
                                              elAttr, elClass, elClass',
                                              elDynAttr,
                                              elementConfig_initialAttributes,
                                              ffor, foldDyn,
                                              inputElementConfig_elementConfig,
                                              inputElementConfig_setValue,
                                              keydown, keyup, mergeWith, text,
                                              wrapDomEvent, (=:))
import           Servant.Common.Req          (reqSuccess)
import           Shared                      (iFa, reqFailure)
import           State                       (EStateUpdate, Message (..),
                                              State (..), updateState)
import           Text.Read                   (readMaybe)
import           Text.Show                   (Show (show))
import Data.Semigroup (Endo(..))

loadingScreen
  :: DomBuilder t m
  => m ()
loadingScreen =
  elClass "div" "mkOverlay" $ do
  iFa "fas fa-spinner fa-spin"
  text " Loading ..."

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

whenJust
  :: forall a t.
  Applicative t
  => Maybe a -> (a -> t ()) -> t ()
whenJust (Just x) a = a x
whenJust Nothing  _ = pure ()

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
  dyn_ $ ffor dynMsg $ \mMsg -> whenJust mMsg $ \Message{..} ->
    let spanClose =
          fmap fst $ elClass' "span" "close" $ iFa "fas fa-times"
    in  elClass "div" "msgOverlay" $ do
          elClose <- spanClose
          let eClose = domEvent Click elClose
          updateState $ eClose $> (field @"stMsg" .~ Nothing)
          el "div" $ text msgCaption
          el "span" $ text msgBody

if' :: Monoid a => Bool -> a -> a
if' True  x = x
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
settings = mdo
  (eFile, eReset) <- elClass "div" "dropdown" $ do
    elClass "span" "dropdown-button" $ iFa "fas fa-cog"
    elClass "div" "dropdown-content" $ (,)
      <$> elClass "span" "hiddenFileInput" (do text "Upload your plover.cfg"
                                               elFileInput $ eReset $> "")
      <*> (domEvent Click . fst <$> el' "span" (text "Reset"))

  updateState $ eReset $> (field @"stPloverCfg" .~ def)

  eReqResult <- postRender $ do
    fileReader <- liftJSM newFileReader
    let encoding = Just ("utf8" :: String)
    performEvent_ $ eFile <&> \f -> readAsText fileReader (Just f) encoding
    eText <- fmap catMaybes $ wrapDomEvent fileReader (`on` load) $ liftJSM $ do
      v <- getResult fileReader
      (fromJSVal <=< toJSVal) v
    dynEitherText <- holdDyn (Left "no file") (Right . Text.unpack <$> eText)
    postConfigNew dynEitherText (void eText)

  let eReqSuccess = mapMaybe reqSuccess eReqResult
      eReqFailure = mapMaybe reqFailure eReqResult

  updateState $ eReqFailure <&> \str ->
    let msgCaption = "Error when loading file"
        msgBody = "Did you upload a proper .cfg file?\n" <> str
    in    (field @"stMsg" .~ Just Message{..})
        . (field @"stPloverCfg" .~ def)

  let compatibleSystems =
        ["Palantype", "Possum Palantype", "Possum Palantype German"]
      isCompatible system = system `elem` compatibleSystems

  updateState $ eReqSuccess <&> \ploverCfg@PloverCfg{..} ->
        appEndo $ mconcat
          [ Endo $ field @"stPloverCfg" .~ ploverCfg
          , if' (not $ null pcfgUnrecognizedQwertys) $
              let msgCaption = "Unrecognized qwerty keys"
                  msgBody = "Your key map contains unrecognized entries:\n"
                         <> Text.intercalate "\n"
                              (Text.pack <$> pcfgUnrecognizedQwertys)
              in  Endo $ field @"stMsg" .~ Just Message{..}
          , if' (not $ null pcfgUnrecognizedStenos) $
              let msgCaption = "Unrecognized steno keys"
                  msgBody = "Your key map contains unrecognized entries:\n"
                         <> Text.intercalate "\n"
                              (Text.pack <$> pcfgUnrecognizedStenos)
              in  Endo $ field @"stMsg" .~ Just Message{..}
          , if' (isCompatible pcfgSystem) $
              let msgCaption = "Incompatible system"
                  msgBody    = "Your system is " <> pcfgMachine
                            <> "\nCompatible systems at the moment are\n"
                            <> Text.intercalate "\n" compatibleSystems
              in  Endo $ field @"stMsg" .~ Just Message{..}
          ]

  dynShowKeyboard <- asks (stShowKeyboard <$>)
  dyn_ $ dynShowKeyboard <&> \showKeyboard -> do
    (s, _) <- if showKeyboard
      then elClass' "span" "btnToggleKeyboard keyboardVisible" $
             iFa "far fa-keyboard"
      else elClass' "span" "btnToggleKeyboard keyboardHidden" $
             iFa "fas fa-keyboard"
    updateState $ domEvent Click s $> (field @"stShowKeyboard" %~ not)

  setRoute $ eReqSuccess $> FrontendRoute_Main :/ ()

data KeyState
  = KeyStateDown PTChar
  | KeyStateUp PTChar

stenoInput
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Dynamic t State) m
  )
  => m ()
stenoInput = do
  dynPloverCfg <- asks (stPloverCfg <$>)
  dyn_ $ dynPloverCfg <&> \PloverCfg{..} -> el "div" $ mdo

    kbInput <- elHiddenInput
    let keyChanges = pcfgLsKeySteno <&> \(qwertyKey, stenoKey) ->
          [ keydown qwertyKey kbInput $> [KeyStateDown stenoKey]
          , keyup   qwertyKey kbInput $> [KeyStateUp   stenoKey]
          ]

        eKeyChange = mergeWith (<>) $ concat keyChanges

        register :: [KeyState] -> Set PTChar -> Set PTChar
        register es set' = foldl acc set' es
          where
            acc s (KeyStateDown k) = Set.insert k s
            acc s (KeyStateUp   k) = Set.delete k s

    dynPressedKeys <- foldDyn register Set.empty eKeyChange

    elStenoOutput dynPressedKeys

    dynShowKeyboard <- asks (stShowKeyboard <$>)
    dyn_ $ dynShowKeyboard <&> \visible -> when visible $
      elPTKeyboard pcfgMapStenoKeys dynPressedKeys pcfgSystem

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

          attrs = ffor dynPressedKeys $ \set' ->
            "colspan" =: colspan <>
            if Set.member cell set'
              then "class" =: "pressed"
              else mempty
      in  if Map.member cell stenoKeys
            then elDynAttr "td" attrs $ do
                   elClass "div" "steno "$ text $ showKey cell
                   elClass "div" "qwerty "$ text $ showQwerties mQwertyKeys
            else elAttr "td" ("colspan" =: colspan <> "class" =: "gap") blank

elHiddenInput
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , PostBuild t m
  , MonadFix m
  )
  => m (InputElement EventResult (DomBuilderSpace m) t)
elHiddenInput = mdo
  dyn_ $ _inputElement_hasFocus i <&> \case
    True  -> elClass "span" "clickMe" $ text "Type!"
    False -> elClass "span" "clickMe red" $ text "Click me!"
  i <- inputElement $ def
    & inputElementConfig_elementConfig
    . elementConfig_initialAttributes
    .~ ("readonly" =: "readonly")
  pure i

elStenoOutput
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , PostBuild t m
  )
  => Dynamic t (Set PTChar)
  -> m ()
elStenoOutput dynPressedKeys =
  elClass "span" "stenoOutput" $ dynText $
     dynPressedKeys <&> \pressedKeys ->
       if Set.null pressedKeys
         then "..."
         else Text.pack $ unwords $ fmap show $ sort $ Set.toList pressedKeys
