{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Page.Keyboard where

import           Language.Javascript.JSaddle (FromJSVal (fromJSVal),
                                              ToJSVal (toJSVal), liftJSM)
import           State                       (EStateUpdate (..))

import           Obelisk.Frontend            (Frontend (..), ObeliskWidget)
import           Obelisk.Generated.Static    (static)
import           Obelisk.Route               (R)

import           Reflex.Dom.Core             (blank, el, elAttr, text, (=:))

import           Client                      (postConfigNew, postRender)
import           Common.Alphabet             (PTChar (..), showKey)
import           Common.Api                  (PloverCfg (..))
import           Common.Keys                 (fromPlover)
import           Common.Route                (FrontendRoute)
import           Control.Monad               (unless, (<=<))
import           Control.Monad.Fix           (MonadFix)
import qualified Data.Aeson                  as Aeson
import           Data.Foldable               (for_)
import           Data.Function               ((&))
import           Data.Functor                (void, ($>))
import           Data.List                   (sort)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe                  (isNothing, listToMaybe, mapMaybe)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Lazy              as Lazy
import qualified Data.Text.Lazy.Encoding     as Lazy
import           Data.Witherable             (Filterable (catMaybes))
import           GHCJS.DOM                   (currentWindowUnchecked)
import           GHCJS.DOM.FileReader        (getResult, load, newFileReader,
                                              readAsText)
import           GHCJS.DOM.Storage           (getItem, setItem)
import           GHCJS.DOM.Window            (getLocalStorage)
import           JSDOM.EventM                (on)
import           JSDOM.Types                 (File)
import           Obelisk.Route.Frontend      (RoutedT)
import           Reflex.Dom                  (DomBuilder (DomBuilderSpace, inputElement),
                                              EventResult, InputElement (..),
                                              MonadHold (holdDyn),
                                              PerformEvent (performEvent_),
                                              PostBuild, Prerender (prerender),
                                              Reflex (Dynamic, Event, updated),
                                              def, dynText, dyn_, elClass,
                                              elDynAttr,
                                              elementConfig_initialAttributes,
                                              ffor, fmapMaybe, foldDyn,
                                              inputElementConfig_elementConfig,
                                              keydown, keyup, mergeWith,
                                              runEventWriterT, widgetHold_,
                                              wrapDomEvent, (.~))
import           Servant.Common.Req          (ReqResult (..))
import           Text.Read                   (readMaybe)

data KeyState
  = KeyStateDown PTChar
  | KeyStateUp PTChar

pageKeyboard ::
  ( DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Dynamic t State) m
  )
  => m ()
pageKeyboard = el "div" $ mdo
  PloverCfg{..} <- asks
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
          Just key -> [keydown key kb $> [KeyStateDown stenoKey]
                      , keyup  key kb $> [KeyStateUp   stenoKey]
                      ]
          Nothing  -> []

      eKeyChange = mergeWith (<>) $ concat keyChanges

      register :: [KeyState] -> Set PTChar -> Set PTChar
      register es set = foldl acc set es
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

          showQwerties Nothing = ""
          showQwerties (Just ks) = Text.pack $ unwords ks

          attrs = ffor dynPressedKeys $ \set ->
            "colspan" =: colspan <>
            if Set.member cell set
              then "class" =: "pressed"
              else mempty
      in  if Map.member cell stenoKeys
            then elDynAttr "td" attrs $ do
                   elClass "div" "steno "$ text $ showKey cell
                   elClass "div" "qwerty "$ text $ showQwerties mQwertyKeys
            else elAttr "td" ("colspan" =: colspan <> "class" =: "gap") blank
