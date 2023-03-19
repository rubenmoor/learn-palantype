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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module StenoInput where

import           Client                         ( postRender
                                                )
import           Common.Model                   ( AppState(..)
                                                )
import           Common.PloverConfig            ( CfgName(..)
                                                , PloverSystemCfg(..)
                                                , defaultPloverSystemCfg
                                                )
import           Control.Applicative            ( Applicative(..) )
import           Control.Category               ( (<<<)
                                                , (>>>)
                                                , Category((.))
                                                )
import           Control.Lens                   ( At(at)
                                                , non
                                                , view
                                                )
import           Control.Lens.Setter            ( (%~)
                                                , (.~)
                                                )
import           Control.Lens.Wrapped           ( _Wrapped' )
import           Control.Monad                  ( guard
                                                , when, (=<<)
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                )
import           Data.Bool                      ( (&&)
                                                , Bool(..)
                                                , bool
                                                , not
                                                , otherwise
                                                , (||)
                                                )
import           Data.Default                   ( Default(def) )
import           Data.Eq                        ( Eq((==)) )
import           Data.Foldable                  ( Foldable(foldl')
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
import           Data.Functor.Misc              ( Const2(Const2) )
import           Data.Generics.Product          ( field )
import           Data.Int                       ( Int )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( Maybe(..)
                                                , fromMaybe


                                                )
import           Data.Monoid                    ( (<>), Monoid (mempty) )
import           Data.Ord                       ( (>=)
                                                , Ord
                                                )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Semigroup                 ( Endo(..) )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text
                                                , unwords
                                                )
import qualified Data.Text                     as Text
import           Data.Tuple                     ( fst
                                                , snd
                                                )
import           Data.Word                      ( Word )
import           GHC.Real                       ( fromIntegral )
import           GHCJS.DOM.HTMLElement          ( focus )
import           Language.Javascript.JSaddle    ( eval
                                                , liftJSM
                                                )
import           Palantype.Common               ( Chord(..)
                                                , KeyIndex
                                                , Palantype(keyCode)
                                                , SystemLang(..)
                                                , fromChord
                                                , fromIndex
                                                , kiDown
                                                , kiInsert
                                                , kiPageDown
                                                , kiPageUp
                                                , kiUp
                                                , mkChord
                                                )
import qualified Palantype.Common.Dictionary.Numbers
                                               as Numbers
import qualified Palantype.Common.Indices      as KI
import           Palantype.Common.TH            ( failure
                                                )
import qualified Palantype.DE                  as DE
import qualified Palantype.EN                  as EN
import           Reflex.Dom                     ( (=:)
                                                , DomBuilder
                                                    ( DomBuilderSpace
                                                    , inputElement
                                                    )
                                                , DomSpace(addEventSpecFlags)
                                                , EventName
                                                    ( Click
                                                    , Keydown
                                                    , Keyup
                                                    )
                                                , EventResult
                                                , EventSelector(select)
                                                , EventTag(KeydownTag, KeyupTag)
                                                , EventWriter
                                                , HasDomEvent
                                                    ( DomEventType
                                                    , domEvent
                                                    )
                                                , InputElement(..)
                                                , InputElementConfig
                                                , KeyCode
                                                , MonadHold
                                                , PerformEvent(performEvent_)

                                                , PostBuild
                                                , Prerender(Client)
                                                , Reflex
                                                    ( Dynamic
                                                    , Event
                                                    , updated
                                                    )
                                                , blank
                                                , el
                                                , elAttr
                                                , elAttr'
                                                , elClass
                                                , elDynAttr
                                                , elDynClass
                                                , elementConfig_eventSpec
                                                , elementConfig_initialAttributes
                                                , elementConfig_modifyAttributes
                                                , fanMap
                                                , fmapMaybe
                                                , foldDyn
                                                , inputElementConfig_elementConfig
                                                , inputElementConfig_initialChecked
                                                , inputElementConfig_initialValue
                                                , inputElementConfig_setValue
                                                , leftmost
                                                , mergeWith
                                                , preventDefault
                                                , text
                                                , zipDyn, delay, constDyn
                                                )
import           Shared                         ( dynSimple
                                                , iFa
                                                )
import           State                          ( State(..)
                                                , updateState, GetLoadedAndBuilt
                                                )
import           TextShow                       ( TextShow(showt) )
import           Type.Reflection                ( (:~~:)(HRefl)
                                                , eqTypeRep
                                                , typeRep
                                                )
import           Witherable                     ( Filterable
                                                    ( catMaybes
                                                    , filter

                                                    )
                                                )

default (Text)

data KeyUpDown key
    = KeyDown key
    | KeyUp key

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

elStenoInput
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) (Client m)
       , MonadHold t m
       , MonadReader (Dynamic t State) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       )
    => GetLoadedAndBuilt t
    -> m (Event t (Chord key))
elStenoInput getLoadedAndBuilt = do
    let lang = if
          | Just HRefl <- typeRep @key `eqTypeRep` typeRep @EN.Key -> SystemEN
          | Just HRefl <- typeRep @key `eqTypeRep` typeRep @DE.Key -> SystemDE
          | otherwise -> $failure "Key not implemented"
    dynPloverCfg <- asks $ fmap $ stApp >>> stPloverCfg
    dynKeyboardShowQwerty <- asks $ fmap $ stApp >>> stKeyboardShowQwerty
    dynShowKeyboard <- asks $ fmap $ stApp >>> stShowKeyboard
    let dynSystemCfg =
            view (_Wrapped' <<< at lang <<< non defaultPloverSystemCfg) <$> dynPloverCfg
    dynSimple $ zipDyn dynSystemCfg dynKeyboardShowQwerty <&>
        \(pcfg, showQwerty) -> postRender
                     $ elClass "div" "mx-auto border-b border-dotted"
                     $ elStenoInput' lang pcfg showQwerty dynShowKeyboard
  where
    elStenoInput' lang PloverSystemCfg{..} showQwerty dynShowKeyboard = mdo
        let keyChanges = pcfgLsKeySteno <&> \(qwertyKey, kI) ->
                [ keydown qwertyKey kbInput $> [KeyDown $ fromIndex kI]
                , keyup   qwertyKey kbInput $> [KeyUp   $ fromIndex kI]
                ]
            eKeyChange = mergeWith (<>) $ concat keyChanges

            register
              :: [KeyUpDown key]
              -> StateInput key
              -> StateInput key
            register es StateInput{ stiKeysPressed, stiKeysDown } =
                let
                    stiKeysPressed' = foldl' accDownUp stiKeysPressed es
                    (stiKeysDown', stiMChord) =
                        if Set.null stiKeysPressed'
                        then
                            ( Set.empty
                            , Just $ mkChord $ Set.elems stiKeysDown
                            )
                        else
                            ( foldl' accDown stiKeysDown es
                            , Nothing
                            )

                in  StateInput
                      { stiKeysPressed = stiKeysPressed'
                      , stiKeysDown    = stiKeysDown'
                      , stiMChord
                      }
                 -- in (setKeys', word', release')
                where
                    accDownUp s (KeyDown k) = Set.insert k s
                    accDownUp s (KeyUp   k) = Set.delete k s
                    accDown   s (KeyDown k) = Set.insert k s
                    accDown   s (KeyUp   _) = s

        dynInput <-
            foldDyn
                register
                (StateInput Set.empty Set.empty Nothing)
                eKeyChange

        let
            dynClass = bool "hidden" "" <$> dynShowKeyboard
        elDynClass "div" dynClass $ case lang of
            -- a bit of a hack to switch to the original Palantype
            -- keyboard layout for English
            -- that original layout I will consider the exception
            SystemEN ->
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

        kbInput <- elShowSteno $ stiKeysDown <$> dynInput
        (elPowerOff, _) <- elAttr' "span"
          (  "class" =: "text-gray-400 cursor-pointer hover:text-red-500"
          <> "title" =: "Switch off interactive input"
          ) $ iFa "fas fa-power-off"
        updateState $ domEvent Click elPowerOff $>
          [field @"stApp" . field @"stKeyboardActive" .~ False]

        -- TODO: doesn't seem to have the desired effect
        let eLostFocus = filter not $ updated $ _inputElement_hasFocus kbInput
        performEvent_ $ eLostFocus $> focus (_inputElement_raw kbInput)

        -- post build auto focus: the post build event happens before the element
        -- is mounted. postmount event waits for pull request to be accepted
        -- https://github.com/reflex-frp/reflex-dom-semui/issues/18
        evLoadedAndBuilt <- delay 0.1 =<< getLoadedAndBuilt
        performEvent_ $ evLoadedAndBuilt $> focus (_inputElement_raw kbInput)

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

-- steno virtual keyboard

data CellContext t key = CellContext
  { ccShowQwerty     :: Bool
  , ccStenoKeys      :: Map KeyIndex [Text]
  , ccDynPressedKeys :: Dynamic t (Set key)
  }

-- css classes

data TextSize = NormalSize | Small | ExtraSmall

data KeyState = Disabled | Enabled BPressed Mode
data BPressed = IsPressed | NotPressed
data Mode = Inactive | Active Submode TextSize
data Submode = ModeNormal | NumberMode | ShiftMode

-- y-offset for the keys for the pinky
data YOffset = HasOffset | NoOffset | ThumbColumn
data Positional = Positional YOffset BHomerow
data BHomerow = IsHomerow | NotHomerow

instance Default Positional where
  def = Positional NoOffset NotHomerow

posHomerow :: Positional
posHomerow = Positional NoOffset IsHomerow

posYOffset :: Positional
posYOffset = Positional HasOffset NotHomerow

mkClassStr
  :: Positional -> KeyState -> Text
-- mkClassStr Inactive _ _ _ = "bg-gray-400 text-grayishblue-900 [&>*:nth-child(1)]:hidden [&>*:nth-child(2)]:hidden [&>*:nth-child(3)]:hidden"
mkClassStr (Positional yOffset bHomerow) enabled =
    unwords [strAll, strKeyState enabled, strYOffset yOffset]
  where
    strAll = "rounded border border-solid border-gray-400 w-[8.11%] h-[25%] text-center"

    strKeyState Disabled = "shadow-[3px_3px_5px_0_#081430] bg-zinc-300 \
                          \[&>*:nth-child(1)]:hidden [&>*:nth-child(2)]:hidden [&>*:nth-child(3)]:hidden"
    strKeyState (Enabled bPressed mode) =
      unwords ["bg-white", strPressed bHomerow bPressed, strMode mode]

    strPressed NotHomerow NotPressed = "shadow-[3px_3px_5px_0_#081430]"
    strPressed NotHomerow IsPressed  = "text-grayishblue-700"
    strPressed IsHomerow  NotPressed = "shadow-[3px_3px_5px_0_#081430,_inset_0_0_8px_6px_#dcdcff]"
    strPressed IsHomerow  IsPressed  = "text-grayishblue-700 shadow-[_inset_0_0_8px_6px_#dcdcff]"

    strMode Inactive         = "text-2xl bg-zinc-300 text-grayishblue-900 [&>*:nth-child(2)]:hidden [&>*:nth-child(3)]:hidden"
    strMode (Active submode size) = unwords ["bg-white", strSubmode submode, strSize size]

    strSubmode ModeNormal = unwords ["[&>*:nth-child(2)]:hidden [&>*:nth-child(3)]:hidden"]
    strSubmode NumberMode = unwords ["[&>*:nth-child(1)]:hidden [&>*:nth-child(3)]:hidden"]
    strSubmode ShiftMode  = unwords ["[&>*:nth-child(1)]:hidden [&>*:nth-child(2)]:hidden"]

    strSize NormalSize = "text-2xl"
    strSize Small      = "text-lg"
    strSize ExtraSmall = "text-[8pt]"

    strYOffset HasOffset   = "relative top-[16px]"
    strYOffset NoOffset    = ""
    strYOffset ThumbColumn = "relative top-[12px]"

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
  -> SystemLang
  -> Bool
  -> m ()
elKeyboard cfgName ccStenoKeys ccDynPressedKeys lang ccShowQwerty =
    let cc = CellContext{..}
    in  elClass "div" "w-[650px] h-[271px] relative mx-auto" do
          elClass "table" "border-separate border-spacing-1 rounded-lg bg-zinc-300 w-full h-full px-3 pt-3 pb-[24px]" do
            el "tr" $ do
                elCell cc 1 1 posYOffset
                elCell cc 4 1 def
                elCell cc 7 1 def
                elCell cc 10 1 def
                elAttr "td" ("colspan" =: "2" <> "class" =: "invisible") blank
                elAttr "td" ("colspan" =: "1" <> "class" =: "invisible w-[2.7%]") blank
                elAttr "td" ("colspan" =: "2" <> "class" =: "invisible") blank
                elCell cc 21 1 def
                elCell cc 24 1 def
                elCell cc 27 1 def
                elCell cc 30 1 posYOffset
            el "tr" $ do
                elCell cc 2 1 $ Positional HasOffset IsHomerow
                elCell cc 5 1 posHomerow
                elCell cc 8 1 posHomerow
                elCell cc 11 1 posHomerow
                elAttr "td" ("colspan" =: "2" <> "class" =: "invisible") blank
                elAttr "td" ("colspan" =: "1" <> "class" =: "invisible w-[2.7%]") blank
                elAttr "td" ("colspan" =: "2" <> "class" =: "invisible") blank
                elCell cc 22 1 posHomerow
                elCell cc 25 1 posHomerow
                elCell cc 28 1 posHomerow
                elCell cc 31 1 $ Positional HasOffset IsHomerow
            el "tr" $ do
                elCell cc 3 1 posYOffset
                elCell cc 6 1 def
                elCell cc 9 1 def
                elCell cc 12 1 def
                elAttr "td" ("colspan" =: "2" <> "class" =: "invisible") blank
                elAttr "td" ("colspan" =: "1" <> "class" =: "invisible w-[2.7%]") blank
                elAttr "td" ("colspan" =: "2" <> "class" =: "invisible") blank
                elCell cc 23 1 def
                elCell cc 26 1 def
                elCell cc 29 1 def
                elCell cc 32 1 posYOffset
            el "tr" $ do
                elAttr "td" ("colspan" =: "2" <> "class" =: "invisible") blank

                -- left thumb
                elCell cc 13 1 $ Positional ThumbColumn NotHomerow
                elCell cc 14 1 $ Positional ThumbColumn NotHomerow
                elCell cc 15 1 $ Positional ThumbColumn IsHomerow
                elCell cc 16 1 $ Positional ThumbColumn NotHomerow

                elAttr "td" ("colspan" =: "1" <> "class" =: "invisible w-[2.7%]") blank

                -- right thumb
                elCell cc 17 1 $ Positional ThumbColumn NotHomerow
                elCell cc 18 1 $ Positional ThumbColumn IsHomerow
                elCell cc 19 1 $ Positional ThumbColumn NotHomerow
                elCell cc 20 1 $ Positional ThumbColumn NotHomerow

                elAttr "td" ("colspan" =: "2" <> "class" =: "invisible") blank


          elClass "div" "absolute top-[12px] text-grayishblue-900 text-center \
                        \w-full" do
              el "div" $ text $ showt lang
              el "div" $ text $ showt cfgName

              ev <- _inputElement_checkedChange <$> inputElement
                  ( def & inputElementConfig_initialChecked .~ ccShowQwerty
                        & inputElementConfig_elementConfig
                            . elementConfig_initialAttributes
                                .~ (  "id"   =: "showQwerties"
                                  <> "type" =: "checkbox"
                                  )
                  )

              updateState $ ev $>
                [ field @"stApp" . field @"stKeyboardShowQwerty" %~ not
                ]

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
elKeyboardEN cfgName ccStenoKeys ccDynPressedKeys =
  let ccShowQwerty = True
      cc = CellContext{..}
  in  elClass "div" "keyboard" $ do
        _ <- el "table" $ do
            el "tr" $ do
                elAttr "td" ("colspan" =: "1" <> "class" =: "invisible") blank
                elCell cc 4 1 def
                elCell cc 7 1 def
                elCell cc 10 1 def
                -- elAttr "td" ("colspan" =: "4" <> "class" =: "invisible") blank
                elAttr "td" ("colspan" =: "1" <> "class" =: "invisible") blank
                elAttr "td" ("colspan" =: "1" <> "class" =: "invisible") blank
                elAttr "td" ("colspan" =: "1" <> "class" =: "invisible") blank
                elAttr "td" ("colspan" =: "1" <> "class" =: "invisible") blank
                elCell cc 21 1 def
                elCell cc 24 1 def
                elCell cc 27 1 def
                elAttr "td" ("colspan" =: "1" <> "class" =: "invisible") blank
            el "tr" $ do
                elCell cc 1 1 def
                elCell cc 5 1 posHomerow
                elCell cc 8 1 posHomerow
                elCell cc 11 1 posHomerow
                elAttr "td" ("colspan" =: "4" <> "class" =: "invisible") blank
                elCell cc 22 1 posHomerow
                elCell cc 25 1 posHomerow
                elCell cc 28 1 posHomerow
                elCell cc 30 1 def
            el "tr" $ do
                elCell cc 2 1 posHomerow
                elCell cc 6 1 def
                elCell cc 9 1 def
                elCell cc 12 1 def
                elAttr "td" ("colspan" =: "4" <> "class" =: "invisible") blank
                elCell cc 23 1 def
                elCell cc 26 1 def
                elCell cc 29 1 def
                elCell cc 31 1 posHomerow
            el "tr" $ do
                elCell cc 3 3 def
                elCell cc 14 1 def
                elCell cc 15 1 posHomerow
                elCell cc 18 2 def
                elCell cc 19 1 posHomerow
                elCell cc 20 1 def
                elCell cc 32 3 def
        elClass "div" "configuration" $ do
            el "div" $ text $ showt SystemEN
            el "div" $ text $ showt cfgName

elCell
  :: forall key t (m :: * -> *)
  . (DomBuilder t m, Palantype key, PostBuild t m)
  => CellContext t key
  -> KeyIndex
  -> Int
  -> Positional
  -> m ()
elCell CellContext{..} i colspan positional =
    let
        (qwerties, dynKeyState) = case Map.lookup i ccStenoKeys of
          Nothing -> ([], constDyn Disabled)
          Just qs -> (qs, ) $ ccDynPressedKeys <&> \s ->
            Enabled (bPressed s) (mode s)

        dynAttrs = dynKeyState <&> \keyState ->
             "class"   =: mkClassStr positional keyState
          <> "colspan" =: showt colspan

    in
        elDynAttr "td" dynAttrs $ do
            let strClass = "font-bold h-[32px] flex justify-center items-center"
            elClass "div" strClass $ text $ Text.singleton $ keyCode k
            elClass "div" strClass $ text strNumberMode
            elClass "div" strClass $ text strShiftMode
            when ccShowQwerty $ elClass "div" "text-xs text-zinc-500 -mt-1"
              $ text $ Text.unwords qwerties
  where
    k = fromIndex i

    (strNumberMode, strShiftMode) = case Numbers.fromIndex i of
        Nothing -> ("", "")
        Just (str, mStrShift) -> (str, fromMaybe "" mStrShift)

    bPressed s = bool NotPressed IsPressed $ k `Set.member` s
    mode setPressedKeys =
      let
          setModeKeys = Set.fromList $ fromIndex <$> [9, 11]
          isModeActive = setModeKeys `Set.isSubsetOf` setPressedKeys

          isShiftPressed = fromIndex 2 `Set.member` setPressedKeys

          isNumberModeActive = isModeActive && not isShiftPressed
          isShiftModeActive  = isModeActive && isShiftPressed

          notANumber = Text.null strNumberMode
          notAShift  = Text.null strShiftMode

          size str | Text.length str >= 3  = ExtraSmall
          size str | Text.length str >= 2  = Small
          size _                           = NormalSize
      in
          if isNumberModeActive && notANumber
              || isShiftModeActive  && notAShift
              || keyCode k == '_'
            then Inactive
            else if
              | isNumberModeActive -> Active  NumberMode (size strNumberMode)
              | isShiftModeActive  -> Active  ShiftMode  (size strShiftMode )
              | otherwise          -> Active  ModeNormal (size "")

elShowSteno
  :: forall key t (m :: * -> *)
  .  ( DomBuilder t m
     , MonadFix m
     , Palantype key
     )
  => Dynamic t (Set key) -- ^
  -> m (InputElement EventResult (DomBuilderSpace m) t)
elShowSteno dynDownKeys = mdo
    let
        strClass = "w-[650px] text-center text-3xl p-2 focus:outline-none"
        eFocus = updated (_inputElement_hasFocus i) <&> \case
            True  -> ("Type!"    , "class" =: Just (strClass <> " " <> "text-grayishblue-900"))
            False -> ("Click me!", "class" =: Just (strClass <> " " <> "text-red-500"      ))
        eTyping = updated dynDownKeys <&> \downKeys -> (,mempty)
            if Set.null downKeys then "..." else showt $ mkChord $ Set.elems downKeys
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
                  <> "class"     =: (strClass <> " " <> "text-red-500")
                   )
                & inputElementConfig_elementConfig . elementConfig_eventSpec
                    %~ addEventSpecFlags
                        (Proxy :: Proxy (DomBuilderSpace m))
                        Keydown
                        (const preventDefault)
    pure i
