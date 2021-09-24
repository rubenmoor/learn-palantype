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

import Data.Tuple (fst)
import           Client                      (postConfigNew, postRender)
import           Common.Alphabet             (PTChar (..), showKey)
import           Common.Api                  (PloverCfg (..))
import           Common.Keys                 (fromPlover)
import           Common.Route                (FrontendRoute (FrontendRoute_Main))
import           Control.Applicative         (Applicative (..))
import           Control.Lens.Setter         (set, (.~))
import           Control.Monad               (unless, (<=<))
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.Reader        (MonadReader, asks)
import Control.Category ( Category(id, (.)) )
import Data.Monoid (Monoid(mempty), (<>))
import           Data.Bool                   ((&&), Bool (..))
import           Data.Default                (Default (def))
import           Data.Foldable               (concat, Foldable(foldl, null), for_)
import           Data.Function               (($), (&))
import           Data.Functor                (void, ($>), (<&>), (<$>), fmap)
import           Data.Generics.Product       (field)
import           Data.List                   (zip, elem, sort)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (Maybe (..), isNothing,
                                              listToMaybe)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.String                 (unwords, String)
import qualified Data.Text                   as Text
import           Data.Witherable             (Filterable (catMaybes, mapMaybe))
import           GHCJS.DOM.FileReader        (getResult, load, newFileReader,
                                              readAsText)
import           JSDOM.EventM                (on)
import           JSDOM.Types                 (File)
import           Language.Javascript.JSaddle (FromJSVal (fromJSVal),
                                              ToJSVal (toJSVal), liftJSM)
import           Obelisk.Route.Frontend      (pattern (:/), R,
                                              SetRoute (setRoute))
import           Reflex.Dom                  (EventTag(ClickTag), Element, DomBuilder (DomBuilderSpace, inputElement),
                                              EventName (Click), EventResult,
                                              EventWriter,
                                              HasDomEvent(DomEventType, domEvent),
                                              InputElement (..),
                                              MonadHold (holdDyn),
                                              PerformEvent (performEvent_),
                                              PostBuild, Prerender,
                                              Reflex (Dynamic, Event, updated),
                                              blank, dynText, dyn_, el, el',
                                              elAttr, elClass,
                                              elClass', elDynAttr,
                                              elementConfig_initialAttributes,
                                              ffor, foldDyn,
                                              inputElementConfig_elementConfig,
                                              keydown, keyup, mergeWith, text,
                                              wrapDomEvent, (=:))
import           Servant.Common.Req          (reqSuccess)
import           Shared                      (iFa, reqFailure)
import           State                       (EStateUpdate, Message (..),
                                              State (..), updateState)
import           Text.Read                   (readMaybe)
import Data.Traversable (Traversable(sequenceA))
import Data.Either (Either(..))
import Data.Eq (Eq((/=)))
import Text.Show (Show(show))

loadingScreen
  :: DomBuilder t m
  => m ()
loadingScreen =
  elClass "div" "mkOverlay" $ do
  iFa "fas fa-spinner fa-spin"
  text " Loading ..."

elFileInput
  :: DomBuilder t m
  => m (Event t File)
elFileInput = do
  i <- inputElement $ def
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

settings
  :: forall t js (m :: * -> *).
  ( DomBuilder t m
  , Prerender js t m
  , SetRoute t (R FrontendRoute) m
  , EventWriter t EStateUpdate m
  )
  => m ()
settings = do
    (eFile, eReset) <- elClass "div" "dropdown" $ do
      elClass "span" "dropdown-button" $ iFa "fas fa-cog"
      elClass "div" "dropdown-content" $ (,)
        <$> elClass "span" "hiddenFileInput" (do text "Upload your plover.cfg"
                                                 elFileInput)
        <*> (domEvent Click . fst <$> el' "span" (text "Reset"))

    el "h3" $ text "Upload Plover config"

    updateState $ eReset $> set (field @"stPloverCfg") def

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
      in    set (field @"stMsg") (Just Message{..})
          . set (field @"stPloverCfg") def

    let compatibleSystems =
          ["Palantype", "Possum Palantype", "Possum Palantype German"]
        isCompatible system = system `elem` compatibleSystems

    updateState $ eReqSuccess <&> \ploverCfg@PloverCfg{..} ->
        set (field @"stPloverCfg") ploverCfg
      . if isCompatible pcfgSystem then id else
          let msgCaption = "Incompatible system"
              msgBody    = "Your system is " <> pcfgMachine
                        <> "\nCompatible systems at the moment are\n"
                        <> Text.intercalate "\n" compatibleSystems
          in  set (field @"stMsg") (Just Message{..})
    setRoute $ eReqSuccess $> FrontendRoute_Main :/ ()

data KeyState
  = KeyStateDown PTChar
  | KeyStateUp PTChar

keyboard ::
  ( DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Dynamic t State) m
  )
  => m ()
keyboard = do
  dynPloverCfg <- asks (stPloverCfg <$>)
  dyn_ $ ffor dynPloverCfg $ \PloverCfg{..} -> el "div" $ mdo
    el "h4" $ text "System"
    el "span" $ text pcfgSystem
    el "h4" $ text "Machine"
    el "span" $ text pcfgMachine
    el "div" $ text "Key map loaded."
    let lsKeySteno =
            mapMaybe (\(k, mV) -> (k,) <$> mV)
          $ Map.toList
          $ readMaybe <$> pcfgKeySteno

        stenoKeys = Map.toList pcfgStenoKeys
        ls = zip stenoKeys $ readMaybe . fst <$> stenoKeys

        unrecognized = mapMaybe (\((k, _), s) ->
          if isNothing s && k /= "no-op" && k /= "arpeggiate"
            then Just k
            else Nothing) ls

        recognizedStenoKeys =
          catMaybes $ ffor ls $ \((_, v), mPTChar) -> (,v) <$> mPTChar

    unless (null unrecognized) $ void $ el "div" $ do
      text "Your key map contains unrecognized entries: "
      for_ unrecognized $ \s -> el "div" $ text $ Text.pack s

    let keyChanges = ffor lsKeySteno $ \(qwertyKey, stenoKey) ->
          case fromPlover qwertyKey of
            Just key -> [ keydown key kb $> [KeyStateDown stenoKey]
                        , keyup   key kb $> [KeyStateUp   stenoKey]
                        ]
            Nothing  -> []

        eKeyChange = mergeWith (<>) $ concat keyChanges

        register :: [KeyState] -> Set PTChar -> Set PTChar
        register es set' = foldl acc set' es
          where
            acc s (KeyStateDown k) = Set.insert k s
            acc s (KeyStateUp   k) = Set.delete k s

    dynPressedKeys <- foldDyn register Set.empty eKeyChange
    kb <- elPTKeyboard (Map.fromList recognizedStenoKeys) dynPressedKeys
    pure ()

elPTKeyboard ::
  ( DomBuilder t m
  , PostBuild t m
  , MonadFix m
  )
  => Map PTChar [String]
  -> Dynamic t (Set PTChar)
  -> m (InputElement EventResult (DomBuilderSpace m) t)
elPTKeyboard stenoKeys dynPressedKeys = elClass "div" "keyboard" $ mdo
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
    dyn_ $ ffor (_inputElement_hasFocus i) $ \case
      True -> el "span" $ text "Type!"
      False -> elClass "span" "red" $ text "Click me!"
    i <- inputElement $ def
      & inputElementConfig_elementConfig
      . elementConfig_initialAttributes
      .~ ("readonly" =: "readonly")
    elClass "span" "steno" $ dynText $
      Text.pack . unwords . fmap show . sort . Set.toList <$> dynPressedKeys
    pure i
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
