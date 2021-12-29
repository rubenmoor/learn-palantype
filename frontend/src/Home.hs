{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE TypeApplications    #-}

module Home where

import           Client                         ( postConfigNew
                                                , postRender
                                                , reqFailure
                                                )
import           Common.Api                     ( CfgName(..)
                                                , PloverSystemCfg(..)
                                                , keyMapToPloverCfg
                                                , lsStenoQwerty
                                                , lsStenoQwertyOrig
                                                , lsStenoQwertz
                                                , showSymbol
                                                )
import           Common.Route                   ( FrontendRoute(..)
                                                , FrontendSubroute_Stage(..)
                                                )
import           Control.Applicative            ( Applicative(..) )
import           Control.Category               ( (>>>)
                                                , Category((.), id)
                                                )
import           Control.Lens                   ( At(at)
                                                , Ixed(ix)
                                                , non
                                                , view
                                                )
import           Control.Lens.Setter            ( (%~)
                                                , (.~)
                                                )
import           Control.Lens.Wrapped           ( _Wrapped' )
import           Control.Monad                  ( (<=<)
                                                , (=<<)
                                                , guard
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.Reader           ( MonadReader(ask)
                                                , ReaderT
                                                , asks
                                                , withReaderT
                                                )
import           Data.Bool                      ( (&&)
                                                , Bool(..)
                                                , bool
                                                , not
                                                )
import           Data.Default                   ( Default(def) )
import           Data.Either                    ( Either(..) )
import           Data.Eq                        ( Eq((/=), (==)) )
import           Data.Foldable                  ( Foldable(foldl, null)
                                                , concat
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
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( Maybe(..)
                                                , listToMaybe

                                                )
import           Data.Monoid                    ( (<>) )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Semigroup                 ( Endo(..) )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.String                    ( String )
import           Data.Text                      ( Text
                                                , unwords
                                                )
import qualified Data.Text                     as Text
import           Data.Tuple                     ( fst
                                                , snd
                                                )
import           Data.Witherable                ( Filterable
                                                    ( catMaybes
                                                    , filter
                                                    , mapMaybe
                                                    )
                                                )
import GHC.Real (fromIntegral)
import           GHCJS.DOM.EventM               ( on )
import           GHCJS.DOM.FileReader           ( getResult
                                                , load
                                                , newFileReader
                                                , readAsText
                                                )
import           GHCJS.DOM.HTMLElement          ( focus )
import           GHCJS.DOM.Types                ( File )
import           Language.Javascript.JSaddle    ( FromJSVal(fromJSVal)
                                                , ToJSVal(toJSVal)
                                                , eval
                                                , liftJSM
                                                )
import           Obelisk.Generated.Static       ( static )
import           Obelisk.Route.Frontend         ( pattern (:/)
                                                , R
                                                , RouteToUrl
                                                , RoutedT
                                                , SetRoute(setRoute)
                                                , mapRoutedT
                                                , routeLink
                                                , subRoute
                                                )
import           Page.Common                    ( elFooter
                                                , rawToggleKeyboard
                                                )
import           Page.Introduction              ( introduction )
import qualified Page.Stage1                   as Stage1
import qualified Page.Stage2                   as Stage2
import           Palantype.Common               (Lang (..),  Chord(..)
                                                , KeyIndex(..)
                                                , Palantype(fromIndex, keyCode)
                                                , mkChord
                                                )
import           Palantype.Common.Dictionary    ( kiDown
                                                , kiUp
                                                )
import qualified Palantype.Common.Indices      as KI
import           Palantype.Common.RawSteno      ( RawSteno(..) )
import qualified Palantype.Common.RawSteno     as Raw
import           Reflex.Dom                     (KeyCode, EventTag(KeyupTag, KeydownTag),  (=:)
                                                , DomBuilder
                                                    ( DomBuilderSpace
                                                    , inputElement
                                                    )
                                                , DomSpace(addEventSpecFlags)
                                                , EventName(Click, Keydown, Keyup)
                                                , EventResult
                                                , EventWriter
                                                , EventWriterT
                                                , HasDomEvent(DomEventType, domEvent)
                                                , InputElement(..)
                                                , InputElementConfig
                                                , MonadHold(holdDyn)
                                                , PerformEvent(performEvent_)
                                                , PostBuild(getPostBuild)
                                                , Prerender(Client)
                                                , Reflex
                                                    ( Dynamic
                                                    , Event
                                                    , updated
                                                    )
                                                , blank
                                                , delay
                                                , dyn_

                                                , el
                                                , el'
                                                , elAttr
                                                , elAttr'
                                                , elClass
                                                , elClass'
                                                , elDynAttr
                                                , elDynClass
                                                , elementConfig_eventSpec
                                                , elementConfig_initialAttributes
                                                , elementConfig_modifyAttributes
                                                , fmapMaybe
                                                , foldDyn
                                                , inputElementConfig_elementConfig
                                                , inputElementConfig_initialValue
                                                , inputElementConfig_setValue
                                                , leftmost
                                                , mergeWith
                                                , preventDefault
                                                , text
                                                , wrapDomEvent
                                                )
import           Servant.Common.Req             ( reqSuccess )
import           Shared                         ( dynSimple
                                                , iFa
                                                , whenJust
                                                )
import           State                          ( Env(..)
                                                , Message(..)
                                                , Navigation(..)
                                                , Stage(..)
                                                , State(..)
                                                , stageDescription
                                                , stageUrl
                                                , updateState
                                                )
import           TextShow                       ( TextShow(showt) )
import Data.Word (Word)

default (Text)

elFileInput :: DomBuilder t m => Event t Text -> m (Event t File)
elFileInput eSet = do
    i <-
        inputElement
        $  def
        &  inputElementConfig_setValue
        .~ eSet
        &  inputElementConfig_elementConfig
        .  elementConfig_initialAttributes
        .~ ("type" =: "file" <> "accept" =: "text/cfg")

    let eFiles = _inputElement_files i
    pure $ mapMaybe listToMaybe $ updated eFiles

message
    :: forall t (m :: * -> *)
     . ( DomBuilder t m
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
    :: forall t js (m :: * -> *)
     . ( DomBuilder t m
       , PostBuild t m
       , Prerender js t m
       , EventWriter t (Endo State) m
       , MonadReader (Dynamic t State) m
       , SetRoute t (R FrontendRoute) m
       )
    => Lang
    -> m ()
settings lang = do
    dynState <- ask

    -- button to toggle keyboard
    let dynShowKeyboard = stShowKeyboard <$> dynState
    dyn_ $ dynShowKeyboard <&> \showKeyboard -> do
        (s, _) <- if showKeyboard
            then elClass' "span" "btnHeader keyboardVisible"
                $ iFa "far fa-keyboard"
            else elClass' "span" "btnHeader keyboardHidden"
                $ iFa "fas fa-keyboard"

        updateState $ domEvent Click s $> [field @"stShowKeyboard" %~ not]

    -- button to show configuration dropdown
    eFile <- elClass "div" "dropdown" $ do
        elClass "span" "dropdown-button" $ iFa "fas fa-cog"
        elClass "div" "dropdown-content" $ mdo

            let dynMCfgName =
                    fmap pcfgName
                        .   view (at lang >>> _Wrapped')
                        .   stPloverCfg
                        <$> dynState
                elCheckmark co =
                    dyn_
                        $   dynMCfgName
                        <&> \cfgName ->
                                elClass "span" "checkmark"
                                    $ if cfgName == Just co
                                      then
                                          text "✓ "
                                      else
                                          blank

            elClass "span" "caption" $ text "Keyboard layout"

            (elQwertz, _) <- elClass' "span" "entry" $ do
                elCheckmark CNQwertzDE
                text "qwertz DE"

            let eQwertz = domEvent Click elQwertz
            updateState
                $  eQwertz
                $> [ field @"stPloverCfg"
                     .  _Wrapped'
                     .  ix lang
                     .~ keyMapToPloverCfg lsStenoQwertz [] "keyboard" CNQwertzDE
                   ]

            (elQwerty, _) <- elClass' "span" "entry" $ do
                elCheckmark CNQwertyEN
                text "qwerty EN"
            let eQwerty = domEvent Click elQwerty
                lsStenoQwertyEN = case lang of
                  EN -> lsStenoQwertyOrig
                  _ -> lsStenoQwerty
            updateState
                $  eQwerty
                $> [ field @"stPloverCfg"
                     .  _Wrapped'
                     .  ix lang
                     .~ keyMapToPloverCfg lsStenoQwertyEN [] "keyboard" CNQwertyEN
                   ]


            eFile' <- elClass "span" "hiddenFileInput entry" $ do
                elCheckmark CNFile
                text "Upload plover.cfg"
                elFileInput $ leftmost [eQwerty, eQwertz] $> ""

            elClass "span" "caption" $ text "Progress"

            (eRP, _) <- elClass' "span" "entry" $ text "Reset"
            updateState $ domEvent Click eRP $> [field @"stProgress" .~ def]

            pure eFile'

    elClass "div" "dropdown" $ do
        elClass "span" "dropdown-button" $ text $ showSymbol lang
        elClass "div" "dropdown-content" $ do
            (eRL, _) <- elClass' "span" "entry" $ text "Switch system"
            let eClickRL = domEvent Click eRL
            updateState $ eClickRL $> [field @"stMLang" .~ Nothing]
            setRoute $ eClickRL $> FrontendRoute_Main :/ ()

    eReqResult <- postRender $ do
        fileReader <- liftJSM newFileReader
        let encoding = Just ("utf8" :: String)
        performEvent_ $ eFile <&> \f -> readAsText fileReader (Just f) encoding
        eText <-
            fmap catMaybes $ wrapDomEvent fileReader (`on` load) $ liftJSM $ do
                v <- getResult fileReader
                (fromJSVal <=< toJSVal) v
        dynEitherText <- holdDyn (Left "no file")
                                 (Right . Text.unpack <$> eText)
        postConfigNew dynEitherText (void eText)

    let eReqSuccess = mapMaybe reqSuccess eReqResult
        eReqFailure = mapMaybe reqFailure eReqResult

    updateState $ eReqFailure <&> \str ->
        let msgCaption = "Error when loading file"
            msgBody    = "Did you upload a proper .cfg file?\n" <> str
        in  [field @"stMsg" .~ Just Message { .. }, field @"stPloverCfg" .~ def]

    updateState $ eReqSuccess <&> \(ln, systemCfg@PloverSystemCfg {..}) ->
        [ field @"stPloverCfg" %~ (_Wrapped' %~ ix ln .~ systemCfg)
        , if null pcfgUnrecognizedQwertys
            then id
            else
                let msgCaption = "Unrecognized qwerty keys"
                    msgBody =
                        "Your key map contains unrecognized entries:\n"
                            <> Text.intercalate "\n" pcfgUnrecognizedQwertys
                in  field @"stMsg" .~ Just Message { .. }
        , if null pcfgUnrecognizedStenos
            then id
            else
                let msgCaption = "Unrecognized steno keys"
                    msgBody =
                        "Your key map contains unrecognized entries:\n"
                            <> Text.intercalate
                                   "\n"
                                   (unRawSteno <$> pcfgUnrecognizedStenos)
                in  field @"stMsg" .~ Just Message { .. }
        ]

data KeyState key
  = KeyStateDown key
  | KeyStateUp key

{-# INLINABLE keydown #-}
keydown
    :: ( Reflex t
       , HasDomEvent t e 'KeydownTag
       , DomEventType e 'KeydownTag ~ Word
       )
    => KeyCode
    -> e
    -> Event t ()
keydown keycode = fmapMaybe (\n -> guard $ fromIntegral n == keycode)
    . domEvent Keydown

{-# INLINABLE keyup #-}
keyup
    :: ( Reflex t
       , HasDomEvent t e 'KeyupTag
       , DomEventType e 'KeyupTag ~ Word
       )
    => KeyCode
    -> e
    -> Event t ()
keyup keycode = fmapMaybe (\n -> guard $ fromIntegral n == keycode)
    . domEvent Keyup

stenoInput
    :: forall js key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) (Client m)
       , MonadHold t m
       , MonadReader (Dynamic t State) m
       , Palantype key
       , PostBuild t m
       , Prerender js t m
       )
    => Lang
    -> m (Event t (Chord key))
stenoInput lang = do

    dynPloverCfg    <- asks (stPloverCfg <$>)
    dynShowKeyboard <- asks (stShowKeyboard <$>)
    dynSimple
        $   dynPloverCfg
        <&> view (_Wrapped' . at lang . non def)
        <&> \PloverSystemCfg {..} ->
                postRender $ elClass "div" "stenoInput" $ mdo
                    let keyChanges = pcfgLsKeySteno <&> \(qwertyKey, kI) ->
                            [ keydown (fromIntegral qwertyKey) kbInput
                                $> [KeyStateDown $ fromIndex kI]
                            , keyup qwertyKey kbInput
                                $> [KeyStateUp $ fromIndex kI]
                            ]

                        eKeyChange = mergeWith (<>) $ concat keyChanges

                        register
                            :: [KeyState key]
                            -> (Set key, Set key, Maybe (Chord key))
                            -> (Set key, Set key, Maybe (Chord key))
                        register es (keys, word, _) =
                            let setKeys'          = foldl accDownUp keys es
                                (word', release') = if Set.null setKeys'
                                    then
                                        ( Set.empty
                                        , Just $ mkChord $ Set.elems word
                                        )
                                    else (foldl accDown word es, Nothing)
                            in  (setKeys', word', release')
                          where
                            accDownUp s (KeyStateDown k) = Set.insert k s
                            accDownUp s (KeyStateUp   k) = Set.delete k s

                            accDown s (KeyStateDown k) = Set.insert k s
                            accDown s (KeyStateUp   _) = s

                    dynInput <- foldDyn register
                                        (Set.empty, Set.empty, Nothing)
                                        eKeyChange
                    let dynPressedKeys = dynInput <&> \(keys, _, _) -> keys
                        dynDownKeys = dynInput <&> \(_, down, _) -> down
                        dynChord = dynInput <&> \(_, _, release) -> release

                    let dynClass = bool "displayNone" "" <$> dynShowKeyboard
                    elDynClass "div" dynClass $ case lang of
                      -- a bit of a hack to switch to the original Palantype
                      -- keyboard layout for English
                      -- that original layout I will consider the exception
                        EN -> elKeyboardEN pcfgName
                                           pcfgMapStenoKeys
                                           dynPressedKeys
                        _ -> elKeyboard pcfgName
                                        pcfgMapStenoKeys
                                        dynPressedKeys
                                        lang

                    kbInput <- elStenoOutput dynDownKeys

                    -- TODO: doesn't seem to have the desired effect
                    let
                        eLostFocus =
                            filter not $ updated $ _inputElement_hasFocus
                                kbInput
                    performEvent_ $ eLostFocus $> focus
                        (_inputElement_raw kbInput)

                    -- post build auto focus: the post build event happens before the element
                    -- is mounted. postmount event waits for pull request to be accepted
                    -- https://github.com/reflex-frp/reflex-dom-semui/issues/18
                    ePb <- delay 0.1 =<< getPostBuild
                    performEvent_ $ ePb $> focus (_inputElement_raw kbInput)

                    let rawDown      = KI.toRaw @key kiDown
                        rawUp        = KI.toRaw @key kiUp
                        eChordAll    = catMaybes $ updated dynChord
                        eChordToggle = filter
                            (\c -> Raw.fromChord c == rawToggleKeyboard lang)
                            eChordAll
                        eChordArrowDown = filter
                            (\c -> Raw.fromChord c == rawDown)
                            eChordAll
                        eChordArrowUp = filter
                            (\c -> Raw.fromChord c == rawUp)
                            eChordAll

                        remainder chord =
                            let raw = Raw.fromChord chord
                            in  raw
                                    /= rawToggleKeyboard lang
                                    && raw
                                    /= rawUp
                                    && raw
                                    /= rawDown
                        eChord = filter remainder eChordAll

                    updateState
                        $  eChordToggle
                        $> [field @"stShowKeyboard" %~ not]

                    -- this is a workaround
                    -- scroll, like focus, is not available in reflex dom
                    -- GHCJS.DOM.Element.scroll relies on GhcjsDomSpace
                    -- GhcjsDomSpace requires the elements to be build post render
                    let
                        jsDown =
                            "let el = document.getElementById(\"content\"); \
                                 \el.scrollBy(0,100)" :: Text
                    performEvent_ $ eChordArrowDown $> void
                        (liftJSM $ eval jsDown)
                    let
                        jsUp =
                            "let el = document.getElementById(\"content\"); \
                               \el.scrollBy(0,-100)" :: Text
                    performEvent_ $ eChordArrowUp $> void (liftJSM $ eval jsUp)

                    pure eChord

elKeyboard
    :: forall key t (m :: * -> *)
     . (DomBuilder t m, Palantype key, PostBuild t m)
    => CfgName
    -> Map KeyIndex [Text]
    -> Dynamic t (Set key)
    -> Lang
    -> m ()
elKeyboard cfgName stenoKeys dynPressedKeys lang =
    elClass "div" "keyboard" $ do
        el "table" $ do
            el "tr" $ do
                elCell stenoKeys dynPressedKeys 1  "1" "pinkyYOffset"
                elCell stenoKeys dynPressedKeys 4  "1" ""
                elCell stenoKeys dynPressedKeys 7  "1" ""
                elCell stenoKeys dynPressedKeys 10 "1" ""
                elAttr "td" ("colspan" =: "2" <> "class" =: "gap")     blank
                elAttr "td" ("colspan" =: "1" <> "class" =: "handgap") blank
                elAttr "td" ("colspan" =: "2" <> "class" =: "gap")     blank
                elCell stenoKeys dynPressedKeys 21 "1" ""
                elCell stenoKeys dynPressedKeys 24 "1" ""
                elCell stenoKeys dynPressedKeys 27 "1" ""
                elCell stenoKeys dynPressedKeys 30 "1" "pinkyYOffset"
            el "tr" $ do
                elCell stenoKeys dynPressedKeys 2  "1" "homerow pinkyYOffset"
                elCell stenoKeys dynPressedKeys 5  "1" "homerow"
                elCell stenoKeys dynPressedKeys 8  "1" "homerow"
                elCell stenoKeys dynPressedKeys 11 "1" "homerow"
                elAttr "td" ("colspan" =: "2" <> "class" =: "gap")     blank
                elAttr "td" ("colspan" =: "1" <> "class" =: "handgap") blank
                elAttr "td" ("colspan" =: "2" <> "class" =: "gap")     blank
                elCell stenoKeys dynPressedKeys 22 "1" "homerow"
                elCell stenoKeys dynPressedKeys 25 "1" "homerow"
                elCell stenoKeys dynPressedKeys 28 "1" "homerow"
                elCell stenoKeys dynPressedKeys 31 "1" "homerow pinkyYOffset"
            el "tr" $ do
                elCell stenoKeys dynPressedKeys 3  "1" "pinkyYOffset"
                elCell stenoKeys dynPressedKeys 6  "1" ""
                elCell stenoKeys dynPressedKeys 9  "1" ""
                elCell stenoKeys dynPressedKeys 12 "1" ""
                elAttr "td" ("colspan" =: "2" <> "class" =: "gap")     blank
                elAttr "td" ("colspan" =: "1" <> "class" =: "handgap") blank
                elAttr "td" ("colspan" =: "2" <> "class" =: "gap")     blank
                elCell stenoKeys dynPressedKeys 23 "1" ""
                elCell stenoKeys dynPressedKeys 26 "1" ""
                elCell stenoKeys dynPressedKeys 29 "1" ""
                elCell stenoKeys dynPressedKeys 32 "1" "pinkyYOffset"
            el "tr" $ do
                elAttr "td" ("colspan" =: "2" <> "class" =: "gap") blank

                -- left thumb
                elCell stenoKeys dynPressedKeys 13 "1" "thumbrow"
                elCell stenoKeys dynPressedKeys 14 "1" "thumbrow"
                elCell stenoKeys dynPressedKeys 15 "1" "homerow thumbrow"
                elCell stenoKeys dynPressedKeys 16 "1" "thumbrow"

                elAttr "td" ("colspan" =: "1" <> "class" =: "handgap") blank

                -- right thumb
                elCell stenoKeys dynPressedKeys 17 "1" "thumbrow"
                elCell stenoKeys dynPressedKeys 18 "1" "homerow thumbrow"
                elCell stenoKeys dynPressedKeys 19 "1" "thumbrow"
                elCell stenoKeys dynPressedKeys 20 "1" "thumbrow"

                elAttr "td" ("colspan" =: "2" <> "class" =: "gap") blank
        elClass "span" "system" $ do
            el "div" $ text $ showt lang
            el "div" $ text $ showt cfgName

-- | original Palantype keyboard layout
-- | unfortunately the keys don't follow the simple order
-- | of top row, home row, bottom row
-- | therefore, I treat the original palantype layout as the exception
elKeyboardEN
    :: forall key t (m :: * -> *)
     . (DomBuilder t m, Palantype key, PostBuild t m)
    => CfgName
    -> Map KeyIndex [Text]
    -> Dynamic t (Set key)
    -> m ()
elKeyboardEN cfgName stenoKeys dynPressedKeys = elClass "div" "keyboard" $ do
    el "table" $ do
        el "tr" $ do
            elAttr "td" ("colspan" =: "1" <> "class" =: "gap") blank
            elCell stenoKeys dynPressedKeys 4  "1" ""
            elCell stenoKeys dynPressedKeys 7  "1" ""
            elCell stenoKeys dynPressedKeys 10 "1" ""
            -- elAttr "td" ("colspan" =: "4" <> "class" =: "gap") blank
            elAttr "td" ("colspan" =: "1" <> "class" =: "gap") blank
            elAttr "td" ("colspan" =: "1" <> "class" =: "gap") blank
            elAttr "td" ("colspan" =: "1" <> "class" =: "gap") blank
            elAttr "td" ("colspan" =: "1" <> "class" =: "gap") blank
            elCell stenoKeys dynPressedKeys 21 "1" ""
            elCell stenoKeys dynPressedKeys 24 "1" ""
            elCell stenoKeys dynPressedKeys 27 "1" ""
            elAttr "td" ("colspan" =: "1" <> "class" =: "gap") blank
        el "tr" $ do
            elCell stenoKeys dynPressedKeys 1  "1" ""
            elCell stenoKeys dynPressedKeys 5  "1" "homerow"
            elCell stenoKeys dynPressedKeys 8  "1" "homerow"
            elCell stenoKeys dynPressedKeys 11 "1" "homerow"
            elAttr "td" ("colspan" =: "4" <> "class" =: "gap") blank
            elCell stenoKeys dynPressedKeys 22 "1" "homerow"
            elCell stenoKeys dynPressedKeys 25 "1" "homerow"
            elCell stenoKeys dynPressedKeys 28 "1" "homerow"
            elCell stenoKeys dynPressedKeys 30 "1" ""
        el "tr" $ do
            elCell stenoKeys dynPressedKeys 2  "1" "homerow"
            elCell stenoKeys dynPressedKeys 6  "1" ""
            elCell stenoKeys dynPressedKeys 9  "1" ""
            elCell stenoKeys dynPressedKeys 12 "1" ""
            elAttr "td" ("colspan" =: "4" <> "class" =: "gap") blank
            elCell stenoKeys dynPressedKeys 23 "1" ""
            elCell stenoKeys dynPressedKeys 26 "1" ""
            elCell stenoKeys dynPressedKeys 29 "1" ""
            elCell stenoKeys dynPressedKeys 31 "1" "homerow"
        el "tr" $ do
            elCell stenoKeys dynPressedKeys 3  "3" ""
            elCell stenoKeys dynPressedKeys 14 "1" ""
            elCell stenoKeys dynPressedKeys 15 "1" "homerow"
            elCell stenoKeys dynPressedKeys 17 "2" ""
            elCell stenoKeys dynPressedKeys 18 "1" "homerow"
            elCell stenoKeys dynPressedKeys 19 "1" ""
            elCell stenoKeys dynPressedKeys 32 "3" ""
    elClass "span" "system" $ do
        el "div" $ text $ showt EN
        el "div" $ text $ showt cfgName

elCell
    :: forall key t (m1 :: * -> *)
     . (DomBuilder t m1, Palantype key, PostBuild t m1)
    => Map KeyIndex [Text]
    -> Dynamic t (Set key)
    -> KeyIndex
    -> Text
    -> Text
    -> m1 ()
elCell stenoKeys dynPressedKeys i colspan strCls =
    case Map.lookup i stenoKeys of
        Nothing -> elAttr "td" ("colspan" =: colspan <> "class" =: "gap") blank
        Just qwerties -> do
            let k        = fromIndex i
                inactive = keyCode k == '_'
                attrs    = dynPressedKeys <&> \set' ->
                    let lsClass = catMaybes
                            [ if Set.member k set'
                                then Just "pressed"
                                else Nothing
                            , if inactive then Just "inactive" else Nothing
                            ]
                    in  "colspan" =: colspan <> "class" =: unwords
                            (strCls : lsClass)
            elDynAttr "td" attrs $ do
                elClass "div" "steno" $ text $ showt k
                elClass "div" "qwerty" $ text $ Text.unwords qwerties

elStenoOutput
    :: forall key t (m :: * -> *)
     . (DomBuilder t m, MonadFix m, Palantype key)
    => Dynamic t (Set key)
    -> m (InputElement EventResult (DomBuilderSpace m) t)
elStenoOutput dynDownKeys = mdo
    let eFocus = updated (_inputElement_hasFocus i) <&> \case
            True  -> ("Type!", "class" =: Just "anthrazit")
            False -> ("Click me!", "class" =: Just "red")
        eTyping = updated dynDownKeys <&> \pressedKeys ->
            if Set.null pressedKeys
                then ("...", "class" =: Nothing)
                else
                    ( showt $ mkChord $ Set.elems pressedKeys
                    , "class" =: Nothing
                    )
        eChange   = leftmost [eFocus, eTyping]
        eSetValue = fst <$> eChange

    i <-
        inputElement
        $  (def :: InputElementConfig EventResult t (DomBuilderSpace m))
        &  inputElementConfig_setValue
        .~ eSetValue
        &  inputElementConfig_elementConfig
        .  elementConfig_modifyAttributes
        .~ (snd <$> eChange)
        &  inputElementConfig_initialValue
        .~ "Click me!"
        &  inputElementConfig_elementConfig
        .  elementConfig_initialAttributes
        .~ (  "readonly"
           =: "readonly"
           <> "autofocus"
           =: "autofocus"
           <> "class"
           =: "red"
           )
        &  inputElementConfig_elementConfig
        .  elementConfig_eventSpec
        %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m))
                             Keydown
                             (const preventDefault)
    pure i

-- Table of Contents

toc
    :: forall js t (m :: * -> *)
     . ( DomBuilder t m
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
    let dynShowTOC    = stShowTOC <$> dynState
        dynShowStage1 = stTOCShowStage1 <$> dynState
        dynShowStage2 = stTOCShowStage2 <$> dynState
        dynShowStage3 = stTOCShowStage3 <$> dynState
    -- button to toggle TOC
    dyn_ $ dynShowTOC <&> \showTOC -> do
        (s, _) <- if showTOC
            then
                elAttr'
                        "span"
                        (  "class"
                        =: "btn TOCVisible"
                        <> "title"
                        =: "Hide Table of Contents"
                        )
                    $ iFa "fas fa-times"
            else
                elAttr'
                        "span"
                        (  "class"
                        =: "btn TOCHidden"
                        <> "title"
                        =: "Show Table of Contents"
                        )
                    $ iFa "fas fa-bars"

        updateState $ domEvent Click s $> [field @"stShowTOC" %~ not]

    let dynClassDisplay = bool "displayNone" "" <$> dynShowTOC
    elDynClass "div" dynClassDisplay $ do
        let dynCleared = stCleared <$> dynState
        dyn_ $ dynCleared <&> \cleared -> do

            let
                elLi stage = do
                    let dynClass =
                            bool "" "bgLightgray" . (== stage) <$> dynCurrent
                    elDynClass "li" dynClass $ do
                        if stage `Set.member` cleared
                            then iFa "fas fa-check"
                            else el "span" $ text "○"
                        routeLink (stageUrl lang stage)
                            $ text
                            $ stageDescription stage

            el "ul" $ do

                elLi Introduction

                (s1, _) <- elClass' "li" "stage" $ do

                    let
                        dynClass =
                            bool "fas fa-caret-right" "fas fa-caret-down"
                                <$> dynShowStage1
                    elDynClass "i" dynClass blank
                    text "Stage 1: The Palantype Alphabet"

                let eClickS1 = domEvent Click s1
                updateState $ eClickS1 $> [field @"stTOCShowStage1" %~ not]

                let dynClassUl1 = bool "displayNone" "" <$> dynShowStage1

                elDynClass "ul" dynClassUl1 $ do

                    elLi Stage1_1
                    elLi Stage1_2
                    elLi Stage1_3
                    elLi Stage1_4
                    elLi Stage1_5
                    elLi Stage1_6
                    elLi Stage1_7

                (s2, _) <- elClass' "li" "stage" $ do

                    let
                        dynClass =
                            bool "fas fa-caret-right" "fas fa-caret-down"
                                <$> dynShowStage2
                    elDynClass "i" dynClass blank
                    text "Stage 2: Syllables and chords"

                let eClickS2 = domEvent Click s2
                updateState $ eClickS2 $> [field @"stTOCShowStage2" %~ not]

                let dynClassUl2 = bool "displayNone" "" <$> dynShowStage2

                elDynClass "ul" dynClassUl2 $ do
                    elLi Stage2_1
                    elLi Stage2_2
                    elLi Stage2_3
                    elLi Stage2_4

landingPage
    :: forall t (m :: * -> *)
     . (DomBuilder t m, EventWriter t (Endo State) m)
    => m ()
landingPage = elClass "div" "landing" $ do
    el "h1" $ text "Type as fast as you speak"
    elClass "div" "action" $ do

        (elDE, _) <- el' "button" $ do
            elClass "div" "icon" $ do
                elFlag "de"
                el "br" blank
                elFlag "at"
                elFlag "ch"
            elClass "div" "countrycode" $ text "DE"
            elClass "div" "description"
                $ text
                      "100'000+ words and growing. A steno system designed for \
                      \the German language."

        (elEN, _) <- el' "button" $ do
            elClass "div" "icon"
                $ elAttr "img" ("src" =: static @"palantype.png") blank
            elClass "div" "countrycode" $ text "EN"
            elClass "div" "description"
                $ text
                      "The original palantype steno system for English, \
                          \brought to your keyboard."

        elClass "div" "button" $ el "div" $ do
            text "Missing a language? Checkout the "
            elAttr
                    "a"
                    ("href" =: "https://github.com/rubenmoor/palantype-tools")
                $ text "source on Github"
            text " to create your own Palantype-style steno system."

        let eClickEN = domEvent Click elEN $> EN
            eClickDE = domEvent Click elDE $> DE
        updateState $ leftmost [eClickEN, eClickDE] <&> \lang ->
            [ field @"stMLang" .~ Just lang
            -- if no progress in map, insert "Introduction"
            , field @"stProgress"
                %~ Map.insertWith (\_ o -> o) lang Introduction
            ]

    elAttr
        "img"
        ("src" =: static @"getstartedhere.png" <> "class" =: "getstartedhere")
        blank

    elClass "div" "usp" $ do
        elClass "div" "caption" $ text "Maximum typing speed"
        elClass "div" "description"
            $ text
                  "Reach a typing speed of up to 300 \
             \words per minute, fast enough to type along as people talk."

    elClass "div" "usp" $ do
        elClass "div" "caption" $ text "Type chords, not letters"
        elClass "div" "description"
            $ text
                  "Input whole words or word parts with a single stroke \
             \using multiple fingers at once. This is why it's so fast \
             \and why it requires a lot of practice."

    elClass "div" "usp" $ do
        elClass "div" "caption" $ text "No special hardware"
        elClass "div" "description" $ do
            text "You will need a keyboard that supports "
            elAttr
                    "a"
                    (  "href"
                    =: "https://en.wikipedia.org/wiki/Rollover_(keyboard)"
                    )
                $ text "N-key roll-over"
            text
                ", to register all the keys that you press simultaneously, and \
             \optionally an ortho-linear key layout."

    elClass "div" "usp" $ do
        elClass "div" "caption" $ text "Free and open-source"
        elClass "div" "description" $ do
            text
                "Find the code on Github and contribute by reporting bugs \
             \and requesting features in the "
            elAttr
                    "a"
                    (  "href"
                    =: "https://github.com/rubenmoor/learn-palantype/issues"
                    )
                $ text "issue tracker"
            text "."

    el "footer" $ do
        text "Want to reach out? Join the "
        elAttr "a" ("href" =: "https://discord.gg/spymr5aCr5")
            $ text "Plover Discord Server"
        text " and find me in #palantype, @gurubm."

  where
    elFlag cc =
        elClass "span" ("flag-icon flag-icon-squared flag-icon-" <> cc) blank

stages
    :: forall js key (proxy :: * -> *) t (m :: * -> *)
     . ( DomBuilder t m
       , MonadHold t m
       , MonadFix m
       , Palantype key
       , PostBuild t m
       , Prerender js t m
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       )
    => proxy key
    -> Lang
    -> RoutedT
           t
           (R FrontendSubroute_Stage)
           (ReaderT (Dynamic t State) (EventWriterT t (Endo State) m))
           ()
stages _ navLang = elClass "div" "box" $ do
    eChord <- el "header" $ do
        settings navLang
        message
        stenoInput navLang

    dynNavigation <- elClass "div" "row" $ mdo

        toc navLang $ navCurrent <$> dynNavigation

        let setEnv
                :: forall
                       a
                 . Maybe Stage
                -> Stage
                -> Maybe Stage
                -> RoutedT
                       t
                       a
                       ( ReaderT
                             (Env t key)
                             (EventWriterT t (Endo State) m)
                       )
                       Navigation
                -> RoutedT
                       t
                       a
                       ( ReaderT
                             (Dynamic t State)
                             (EventWriterT t (Endo State) m)
                       )
                       Navigation
            setEnv navMPrevious navCurrent navMNext = mapRoutedT
                (withReaderT $ \dynState -> Env
                    { envDynState   = dynState
                    , envEChord     = eChord
                    , envNavigation = Navigation { .. }
                    }
                )
        dynNavigation <-
            elAttr "section" ("id" =: "content") $ do
              elClass "div" "scrollTop" $
                  text $ "Up ▲ " <> showt (KI.toRaw @key kiUp)
              d <-
                elClass "div" "content" $ subRoute $ \case
                  FrontendSubroute_Introduction ->
                      setEnv Nothing Introduction (Just Stage1_1) introduction
                  FrontendSubroute_Stage1_1 -> setEnv (Just Introduction)
                                                      Stage1_1
                                                      (Just Stage1_2)
                                                      Stage1.exercise1
                  FrontendSubroute_Stage1_2 -> setEnv (Just Stage1_1)
                                                      Stage1_2
                                                      (Just Stage1_3)
                                                      Stage1.exercise2
                  FrontendSubroute_Stage1_3 -> setEnv (Just Stage1_2)
                                                      Stage1_3
                                                      (Just Stage1_4)
                                                      Stage1.exercise3
                  FrontendSubroute_Stage1_4 -> setEnv (Just Stage1_3)
                                                      Stage1_4
                                                      (Just Stage1_5)
                                                      Stage1.exercise4
                  FrontendSubroute_Stage1_5 -> setEnv (Just Stage1_4)
                                                      Stage1_5
                                                      (Just Stage1_6)
                                                      Stage1.exercise5
                  FrontendSubroute_Stage1_6 -> setEnv (Just Stage1_5)
                                                      Stage1_6
                                                      (Just Stage1_7)
                                                      Stage1.exercise6
                  FrontendSubroute_Stage1_7 -> setEnv (Just Stage1_6)
                                                      Stage1_7
                                                      (Just Stage2_1)
                                                      Stage1.exercise7
                  FrontendSubroute_Stage2_1 -> setEnv (Just Stage1_7)
                                                      Stage2_1
                                                      (Just Stage2_2)
                                                      Stage2.exercise1
                  FrontendSubroute_Stage2_2 -> setEnv (Just Stage2_1)
                                                      Stage2_2
                                                      (Just Stage2_3)
                                                      Stage2.exercise2
                  FrontendSubroute_Stage2_3 -> setEnv (Just Stage2_2)
                                                      Stage2_3
                                                      (Just Stage2_4)
                                                      Stage2.exercise3
                  FrontendSubroute_Stage2_4 ->
                      setEnv (Just Stage2_3) Stage2_4 Nothing Stage2.exercise4
              elClass "div" "scrollBottom" $
                  text $ "Down ▼ " <> showt (KI.toRaw @key kiDown)
              pure d
        pure dynNavigation
    dyn_ $ dynNavigation <&> elFooter navLang
