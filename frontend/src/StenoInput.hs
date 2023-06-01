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

import           Common.Model                   ( StateKeyboard (..), TocNavigation (..)
                                                )
import           Common.PloverConfig            ( CfgName(..)
                                                , PloverSystemCfg(..)
                                                , defaultPloverSystemCfg, PloverCfg
                                                )
import           Control.Applicative            ( Applicative(..) )
import           Control.Category               ( Category((.))
                                                )
import           Control.Lens                   ( At(at)
                                                , non
                                                , (^.)
                                                )
import           Control.Lens.Setter            ( (%~)
                                                , (.~)
                                                )
import           Control.Lens.Wrapped           ( _Wrapped' )
import           Control.Monad                  ( guard
                                                , (=<<)
                                                )
import           Control.Monad.Fix              ( MonadFix )
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
                                                , concat, traverse_
                                                )
import           Data.Function                  ( ($)
                                                , (&)
                                                , const
                                                )
import           Data.Functor                   ( ($>)
                                                , (<$>)
                                                , (<&>)
                                                , fmap
                                                , void, (<$)
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
import           GHCJS.DOM.HTMLElement          ( focus, IsHTMLElement )
import           Language.Javascript.JSaddle    ( eval
                                                , liftJSM, MonadJSM
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
                                                , mkChord, kiLeft, kiRight, kiDelete, kiCtrlNumber, kiChordToInt, mapHierarchyStageIndex, kiBackUp
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
                                                , DomSpace(addEventSpecFlags, RawInputElement)
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

                                                , PerformEvent(..)

                                                , PostBuild

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

                                                , elementConfig_eventSpec
                                                , elementConfig_initialAttributes
                                                , elementConfig_modifyAttributes
                                                , fanMap
                                                , fmapMaybe
                                                , foldDyn
                                                , inputElementConfig_elementConfig

                                                , inputElementConfig_initialValue
                                                , inputElementConfig_setValue
                                                , leftmost
                                                , mergeWith
                                                , preventDefault
                                                , text
                                                , delay, constDyn, holdUniqDyn, MonadHold, TriggerEvent, elDynClass, inputElementConfig_setChecked, tag, current, zipDyn, gate, attach
                                                )
import           Shared                         ( iFa, whenJust, setRouteAndLoading
                                                )
import           State                          ( State(..)
                                                , updateState, Navigation (..), stageUrl, GetLoadedAndBuilt
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
import Obelisk.Route.Frontend (SetRoute, R)
import Common.Route (FrontendRoute)
import qualified Palantype.Common.Dictionary.Commands as Commands
import Data.List (delete)
import qualified Palantype.Common.Dictionary.CommandsFKeys as FKeys
import qualified Palantype.Common.Dictionary.Special as Special
import Control.Monad.IO.Class (MonadIO)

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
    = FanInsert
    | FanDelete
    | FanDown
    | FanUp
    | FanLeft
    | FanRight
    | FanPageUp
    | FanPageDown
    | FanSmallNumber
    | FanBackUp
    | FanOther
    deriving (Eq, Ord)

data ChordContent k
    = ContentNone
    | ContentAnyChord (Chord k)
    | ContentSmallNumber Int

data StateInput key = StateInput
  {
    -- | the set of keys that are currently pressed
    stiKeysPressed :: Set key

  -- | the set of keys that have been pressed since the last
  --   release; a release is the event of no key pressed
  , stiKeysDown    :: Set key

  -- | a key chord, made from the set of keys that have been
  -- pressed since the last release
  -- the difference to `stiKeysDown`: stiMChord changes
  -- state upon release, whereas `stiKeysDown` changes state
  -- every time a new key is pushed down AND upon release
  , stiMChord      :: Maybe (Chord key)
  }

elStenoInput :: forall key (m :: * -> *) t.
  ( DomBuilder t m
  , EventWriter t (Endo State) m
  , IsHTMLElement (RawInputElement (DomBuilderSpace m))
  , MonadJSM (Performable m)
  , PerformEvent t m
  , Palantype key
  , PostBuild t m
  , SetRoute t (R FrontendRoute) m, MonadHold t m, MonadFix m
  , TriggerEvent t m
  )
  => Dynamic t StateKeyboard
  -> PloverCfg
  -> Navigation
  -> GetLoadedAndBuilt t
  -> m (Event t (Maybe (Chord key)))
elStenoInput dynStateKeyboard ploverCfg Navigation{..} getLoadedAndBuilt = mdo
    let lang = if
          | Just HRefl <- typeRep @key `eqTypeRep` typeRep @EN.Key -> SystemEN
          | Just HRefl <- typeRep @key `eqTypeRep` typeRep @DE.Key -> SystemDE
          | otherwise -> $failure "Key not implemented"

        PloverSystemCfg{..} = ploverCfg ^. _Wrapped' . at lang . non defaultPloverSystemCfg

        keyChanges = pcfgLsKeySteno <&> \(qwertyKey, kI) ->
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

    elDynClass "div" (bool "hidden" "" . stShow <$> dynStateKeyboard) $ case lang of
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
                getLoadedAndBuilt
                dynStateKeyboard
                pcfgMapStenoKeys
                (stiKeysPressed <$> dynInput)
                lang

    let
        dynKeysDown = stiKeysDown <$> dynInput

    evCtrlNumber <- fmap updated $ holdUniqDyn $ dynKeysDown <&> \s ->
      Text.init (showt $ KI.toRaw @key kiCtrlNumber) `Text.isPrefixOf` showt (fromChord $ mkChord $ Set.elems s)

    kbInput <- elShowSteno dynKeysDown

    (elPowerOff, _) <- elAttr' "span"
      (  "class" =: "text-gray-400 cursor-pointer hover:text-red-500"
      <> "title" =: "Switch off interactive input"
      ) $ iFa "fas fa-power-off"
    updateState $ domEvent Click elPowerOff $>
      [field @"stApp" . field @"stKeyboard". field @"stActive" .~ False]

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
            | fromChord c == KI.toRaw @key kiInsert -> Map.singleton FanInsert     ContentNone
            | fromChord c == KI.toRaw @key kiDelete -> Map.singleton FanDelete     ContentNone
            | fromChord c == KI.toRaw @key kiUp     -> Map.singleton FanUp         ContentNone
            | fromChord c == KI.toRaw @key kiDown   -> Map.singleton FanDown       ContentNone
            | fromChord c == KI.toRaw @key kiLeft   -> Map.singleton FanLeft       ContentNone
            | fromChord c == KI.toRaw @key kiRight  -> Map.singleton FanRight      ContentNone
            | fromChord c == KI.toRaw @key kiPageUp -> Map.singleton FanPageUp     ContentNone
            | fromChord c == KI.toRaw @key kiPageDown -> Map.singleton FanPageDown ContentNone
            | Just n <- (kiChordToInt $ KI.fromChord c) -> Map.singleton FanSmallNumber $ ContentSmallNumber n
            | KI.fromChord c == kiBackUp            -> Map.singleton FanBackUp     ContentNone
            | otherwise                             -> Map.singleton FanOther      $ ContentAnyChord c
        eChordToggle      = select selector (Const2 FanInsert     ) $> ()
        evChordToc        = select selector (Const2 FanDelete     ) $> ()
        eChordDown        = select selector (Const2 FanDown       ) $> ()
        eChordUp          = select selector (Const2 FanUp         ) $> ()
        eChordLeft        = select selector (Const2 FanLeft       ) $> ()
        eChordRight       = select selector (Const2 FanRight      ) $> ()
        eChordPageDown    = select selector (Const2 FanPageDown   ) $> ()
        eChordPageUp      = select selector (Const2 FanPageUp     ) $> ()
        eChordSmallNumber = select selector (Const2 FanSmallNumber) <&> \case
          (ContentSmallNumber n) -> n
          _                      -> $failure "unexpected"
        evBackUp          = select selector (Const2 FanBackUp     ) $> ()
        eChordOther       = select selector (Const2 FanOther      ) <&> \case
          (ContentAnyChord    c) -> c
          _                      -> $failure "unexpected"

    updateState $ eChordToggle $> [field @"stApp" . field @"stKeyboard" . field @"stShow" %~ not]
    updateState $ evChordToc   $> [field @"stApp" . field @"stToc" . field @"stVisible" %~ not ]

    let
        behTocNav = stTocNavigation <$> current dynStateKeyboard
        behTocNavIsToplevel = (TocNavToplevel ==) <$> behTocNav
    updateState $ gate behTocNavIsToplevel evCtrlNumber <&> \b ->
      [ field @"stApp" . field @"stToc" . field @"stShowToplevelSteno" .~ b ]

    updateState $ gate (not <$> behTocNavIsToplevel) evBackUp $>
      [ field @"stApp" . field @"stToc"      . field @"stShowSublevelSteno" .~ False
      , field @"stApp" . field @"stKeyboard" . field @"stTocNavigation"     .~ TocNavToplevel
      ]
    let evGotoStage = catMaybes $ attach behTocNav eChordSmallNumber <&> \case
          (TocNavSublevel t, i) -> Map.lookup (t, Just i) $ mapHierarchyStageIndex @key
          (TocNavToplevel  , i) -> Map.lookup (i, Nothing) $ mapHierarchyStageIndex @key

    updateState $ evGotoStage $>
      [ field @"stApp" . field @"stToc"      . field @"stShowSublevelSteno" .~ False
      , field @"stApp" . field @"stKeyboard" . field @"stTocNavigation"     .~ TocNavToplevel
      ]
    setRouteAndLoading $ stageUrl @key <$> evGotoStage

    updateState $ gate behTocNavIsToplevel eChordSmallNumber <&> \n ->
      [ field @"stApp" . field @"stToc" %~ ( field @"stShowStage" .~ Set.singleton n)
                                         . ( field @"stShowSublevelSteno" .~ True)
                                         . ( field @"stShowToplevelSteno" .~ False)
      , field @"stApp" . field @"stKeyboard" . field @"stTocNavigation" .~ TocNavSublevel n
      ]

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

    whenJust navMNext     $ \nxt -> setRouteAndLoading $ eChordRight $> stageUrl @key nxt
    whenJust navMPrevious $ \prv -> setRouteAndLoading $ eChordLeft  $> stageUrl @key prv
    pure $ leftmost [Just <$> eChordOther, Nothing <$ evBackUp]

-- steno virtual keyboard

data CellContext t key = CellContext
  { ccDynShowQwerty  :: Dynamic t QwertySwitch
  , ccDynModes       :: Dynamic t SpecialSwitch
  , ccStenoKeys      :: Map KeyIndex [Text]
  , ccDynPressedKeys :: Dynamic t (Set key)
  }

-- css classes

data TextSize = NormalSize | Small | ExtraSmall

data KeyState = Disabled | Enabled BPressed Mode
data BPressed = IsPressed | NotPressed
data Mode = Inactive | Active Submode TextSize
data Submode
  = ModeNormal
  | NumberMode
  | ShiftMode
  | CommandMode
  | FKeysMode
  | SpecialMode
  | SpecialShiftMode
data SpecialSwitch = SpecialActive | SpecialInactive
  deriving (Eq)

data QwertySwitch = QwertyVisible | QwertyHidden

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
  :: Positional -> KeyState -> SpecialSwitch -> QwertySwitch -> Text
mkClassStr (Positional yOffset bHomerow) enabled sswitch qwertySwitch =
    unwords
      [ strAll
      , strKeyState enabled
      , strYOffset yOffset
      , strShowQwerty
      ]
  where
    strShowQwerty = case qwertySwitch of
      QwertyVisible -> ""
      QwertyHidden  -> "[&>*:nth-child(8)]:hidden"

    strAll = "rounded border border-solid border-gray-400 w-[8.11%] h-[25%] text-center"

    strKeyState Disabled = "shadow-[3px_3px_5px_0_#081430] bg-zinc-200 "
      <> "[&>*:nth-child(1)]:hidden \
         \[&>*:nth-child(2)]:hidden \
         \[&>*:nth-child(3)]:hidden \
         \[&>*:nth-child(4)]:hidden \
         \[&>*:nth-child(5)]:hidden \
         \[&>*:nth-child(6)]:hidden \
         \[&>*:nth-child(7)]:hidden"
    strKeyState (Enabled bPressed mode) =
      unwords [strPressed bHomerow bPressed, strMode mode]

    strPressed NotHomerow NotPressed = "shadow-[3px_3px_5px_0_#081430]"
    strPressed NotHomerow IsPressed  = "text-grayishblue-700"
    strPressed IsHomerow  NotPressed = "shadow-[3px_3px_5px_0_#081430,_inset_0_0_8px_6px_#dcdcff]"
    strPressed IsHomerow  IsPressed  = "text-grayishblue-700 shadow-[_inset_0_0_8px_6px_#dcdcff]"

    strMode Inactive = "bg-zinc-200 text-2xl "
      <> "[&>*:nth-child(1)]:invisible \
         \[&>*:nth-child(2)]:hidden \
         \[&>*:nth-child(3)]:hidden \
         \[&>*:nth-child(4)]:hidden \
         \[&>*:nth-child(5)]:hidden \
         \[&>*:nth-child(6)]:hidden \
         \[&>*:nth-child(7)]:hidden"
    strMode (Active submode size) = unwords ["bg-white", strSubmode sswitch submode, strSize size]

    strSubmode SpecialInactive _                = cssHideAllButNthChild 7 1
    strSubmode SpecialActive   ModeNormal       = cssHideAllButNthChild 7 1
    strSubmode SpecialActive   NumberMode       = cssHideAllButNthChild 7 2
    strSubmode SpecialActive   ShiftMode        = cssHideAllButNthChild 7 3
    strSubmode SpecialActive   CommandMode      = cssHideAllButNthChild 7 4
    strSubmode SpecialActive   FKeysMode        = cssHideAllButNthChild 7 5
    strSubmode SpecialActive   SpecialMode      = cssHideAllButNthChild 7 6
    strSubmode SpecialActive   SpecialShiftMode = cssHideAllButNthChild 7 7

    strSize NormalSize = "text-2xl"
    strSize Small      = "text-lg"
    strSize ExtraSmall = "text-[8pt]"

    strYOffset HasOffset   = "relative top-[16px]"
    strYOffset NoOffset    = ""
    strYOffset ThumbColumn = "relative top-[12px]"

    cssHideAllButNthChild :: Int -> Int -> Text
    cssHideAllButNthChild max d =
      Text.unwords $ delete d [1..max] <&> \i ->
        "[&>*:nth-child(" <> showt i <> ")]:hidden"

elKeyboard
  :: forall key t (m :: * -> *)
  .  ( DomBuilder t m
     , EventWriter t (Endo State) m
     , MonadHold t m
     , MonadIO (Performable m)
     , Palantype key
     , PerformEvent t m
     , PostBuild t m
     , TriggerEvent t m
     )
  => CfgName
  -> GetLoadedAndBuilt t
  -> Dynamic t StateKeyboard
  -> Map KeyIndex [Text]
  -> Dynamic t (Set key)
  -> SystemLang
  -> m ()
elKeyboard cfgName getLoadedAndBuilt dynStateKeyboard ccStenoKeys ccDynPressedKeys lang =
    let
         ccDynShowQwerty = bool QwertyHidden    QwertyVisible . stShowQwerty <$> dynStateKeyboard
         ccDynModes      = bool SpecialInactive SpecialActive . stModes      <$> dynStateKeyboard
         cc = CellContext{..}
    in  elClass "div" "w-[650px] h-[271px] relative mx-auto" do
          elClass "table" "border-separate border-spacing-1 rounded-lg bg-zinc-200 w-full h-full px-3 pt-3 pb-[24px]" do
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

              evQwertiesInitial <- tag (stShowQwerty <$> current dynStateKeyboard) <$> getLoadedAndBuilt
              evToggleQwerties <- _inputElement_checkedChange <$> inputElement
                  ( def & inputElementConfig_setChecked .~ evQwertiesInitial
                        & inputElementConfig_elementConfig
                            . elementConfig_initialAttributes
                                .~ (  "id"  =: "showQwerties"
                                  <> "type" =: "checkbox"
                                  )
                  )
              elAttr "label" ("for" =: "showQwerties") $ text "Show qwerty keys"
              updateState $ evToggleQwerties $>
                [ field @"stApp" . field @"stKeyboard" . field @"stShowQwerty" %~ not
                ]

              el "br" blank
              evModesInitial <- tag (stModes <$> current dynStateKeyboard) <$> getLoadedAndBuilt
              evToggleModes <- _inputElement_checkedChange <$> inputElement
                  ( def & inputElementConfig_setChecked .~ evModesInitial
                        & inputElementConfig_elementConfig
                            . elementConfig_initialAttributes
                                .~ (  "id"   =: "modes"
                                   <> "type" =: "checkbox"
                                   )
                  )
              elAttr "label" ("for" =: "modes") $ text "Show special modes"
              updateState $ evToggleModes $>
                [ field @"stApp" . field @"stKeyboard" . field @"stModes" %~ not
                ]

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
  let ccDynShowQwerty = constDyn QwertyVisible
      ccDynModes = constDyn SpecialInactive
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
          Just qs -> (qs, ) $ zipDyn ccDynPressedKeys ccDynModes <&> \(s, modes) ->
            Enabled (bPressed s) (mode s modes)

        dynAttrs = zipDyn (zipDyn dynKeyState ccDynModes) ccDynShowQwerty <&>
          \((keyState, modes), showQwerty) ->
               "class"   =: mkClassStr positional keyState modes showQwerty
            <> "colspan" =: showt colspan

    in
        elDynAttr "td" dynAttrs $ do
            traverse_
                ( elClass "div" "font-bold h-[32px] flex justify-center \
                                \items-center" . text
                )
                [ Text.singleton $ keyCode k
                , strNumberMode
                , strShiftMode
                , strCommandMode
                , strFKeysMode
                , strSpecialMode
                , strSpecialShiftMode
                ]
            elClass "div" "text-xs text-zinc-500 -mt-1"
              $ text $ Text.unwords qwerties
  where
    k = fromIndex i

    (strNumberMode, strShiftMode) = case Numbers.fromIndex i of
        Nothing -> ("", "")
        Just (str, mStrShift) -> (str, fromMaybe "" mStrShift)
    strCommandMode = fromMaybe "" $ Commands.fromIndex i
    strFKeysMode   = fromMaybe "" $ FKeys.fromIndex i
    (strSpecialMode, strSpecialShiftMode) = fromMaybe ("", "") $ Special.fromIndex i

    bPressed s = bool NotPressed IsPressed $ k `Set.member` s
    mode setPressedKeys modes =
      let
          isNumberModeAnyActive = Set.fromList (fromIndex <$> [9, 11])
            `Set.isSubsetOf` setPressedKeys
          isCommandModeActive = Set.fromList (fromIndex <$> [8, 11])
            `Set.isSubsetOf` setPressedKeys
          isFKeysModeActive = Set.fromList (fromIndex <$> [7, 11])
            `Set.isSubsetOf` setPressedKeys
          isLeftShiftPressed = fromIndex 2 `Set.member` setPressedKeys
          isSpecialModeAnyActive = fromIndex 25 `Set.member` setPressedKeys
          isRightShiftPressed = fromIndex 31 `Set.member` setPressedKeys

          isNumberModeActive  = isNumberModeAnyActive && not isLeftShiftPressed
          isNumberShiftModeActive   = isNumberModeAnyActive && isLeftShiftPressed

          isSpecialModeActive = isSpecialModeAnyActive && not isRightShiftPressed
          isSpecialModeShiftActive = isSpecialModeAnyActive && isRightShiftPressed

          size str | Text.length str >= 3  = ExtraSmall
          size str | Text.length str >= 2  = Small
          size _                           = NormalSize
      in
          if keyCode k == '_'
          then Inactive
          else if modes == SpecialInactive
               then Active ModeNormal (size "")
               else if
                         (isNumberModeActive       && Text.null strNumberMode      )
                      || (isNumberShiftModeActive  && Text.null strShiftMode       )
                      || (isCommandModeActive      && Text.null strCommandMode     )
                      || (isFKeysModeActive        && Text.null strFKeysMode       )
                      || (isSpecialModeActive      && Text.null strSpecialMode     )
                      || (isSpecialModeShiftActive && Text.null strSpecialShiftMode)
                    then Inactive
                    else if
                      | isNumberModeActive       -> Active NumberMode  (size strNumberMode)
                      | isNumberShiftModeActive  -> Active ShiftMode   (size strShiftMode )
                      | isCommandModeActive      -> Active CommandMode (size strCommandMode)
                      | isFKeysModeActive        -> Active FKeysMode   (size strFKeysMode)
                      | isSpecialModeActive      -> Active SpecialMode (size strSpecialMode)
                      | isSpecialModeShiftActive -> Active SpecialShiftMode (size strSpecialShiftMode)
                      | otherwise                -> Active ModeNormal  (size "")

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
