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

module Home where

import           Client                         ( getMaybeAuthData
                                                , postConfigNew
                                                , postEventViewPage
                                                , postRender
                                                , request
                                                )
import           Common.Api                     ( showSymbol )
import           Common.Model                   ( AppState(..)
                                                , Message(..), TextLang (TextEN), StateKeyboard (..), StateToc (..)
                                                )
import           Common.PloverConfig            ( CfgName(..)
                                                , PloverSystemCfg(..)
                                                , defaultPloverCfg
                                                , keyMapToPloverCfg
                                                , lsStenoQwerty
                                                , lsStenoQwertyOrig
                                                , lsStenoQwertz
                                                )
import           Common.Route                   ( FrontendRoute(..)
                                                , FrontendRoute_AuthPages(..)
                                                , showRoute
                                                )
import           Control.Applicative            ( Applicative(..) )
import           Control.Category               ( Category((.), id), (>>>)
                                                )
import           Control.Lens                   ( At(at)
                                                , Ixed(ix)
                                                , view
                                                )
import           Control.Lens.Setter            ( (%~)
                                                , (.~)
                                                , (?~)
                                                )
import           Control.Lens.Wrapped           ( _Wrapped' )
import           Control.Monad                  ( (<=<), (=<<))
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , ask
                                                , asks
                                                , withReaderT
                                                )
import           Data.Bool                      ( bool
                                                , not
                                                , otherwise, Bool (..)
                                                )
import           Data.Default                   ( Default(def) )
import           Data.Either                    ( Either(..)
                                                , either
                                                )
import           Data.Eq                        ( Eq((==)) )
import           Data.Foldable                  ( traverse_, Foldable (null))
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
import           Data.Int                       ( Int )
import           Data.Maybe                     ( Maybe(..)
                                                , listToMaybe
                                                , maybe
                                                )
import           Data.Monoid                    ( (<>) )
import qualified Data.Text                     as Text
import           Language.Javascript.JSaddle    ( FromJSVal(fromJSVal)
                                                , ToJSVal(toJSVal)
                                                , liftJSM
                                                )
import           Obelisk.Generated.Static       ( static )
import           Obelisk.Route.Frontend         ( pattern (:/)
                                                , R
                                                , RouteToUrl
                                                , RoutedT
                                                , SetRoute
                                                , askRoute
                                                , mapRoutedT, Routed

                                                )
import           Page.Common                    ( elFooter )
import           Page.Introduction              ( introduction )
import qualified Page.Patterns                 as Patterns
import qualified Page.Stage1                   as Stage1
import           Page.Stage15.CommandKeys       ( commandKeys )
import           Page.Stage15.Fingerspelling    ( fingerspelling )
import           Page.Stage15.NumberMode        ( numberMode )
import           Page.Stage15.PloverCommands    ( ploverCommands )
import           Page.Stage15.SpecialCharacters ( specialCharacters )
import qualified Page.Stage2                   as Stage2
import           Page.StageGeneric              ( getGenericExercise )
import           Palantype.Common               ( StageIndex
                                                , StageSpecialGeneric(..)
                                                , SystemLang(..)
                                                , kiDown
                                                , kiInsert
                                                , kiPageDown
                                                , kiPageUp
                                                , kiUp
                                                , mkStageIndex, getSystemLang, Palantype, Stage (..), kiDelete, kiFromSmallNumber
                                                )
import           Palantype.Common.TH            ( failure
                                                , fromJust
                                                )
import qualified Palantype.DE                  as DE
import qualified Palantype.EN                  as EN
import           Reflex.Dom                     ( (=:)
                                                , DomBuilder (..)
                                                , EventName ( Click)
                                                , EventWriter
                                                , EventWriterT
                                                , InputElement(..)
                                                , MonadHold(holdDyn)
                                                , PerformEvent(performEvent_)
                                                , Performable
                                                , PostBuild
                                                , Prerender (Client)
                                                , Reflex
                                                    ( Dynamic
                                                    , Event
                                                    , updated
                                                    )
                                                , TriggerEvent

                                                , blank
                                                , current
                                                , dyn_
                                                , el
                                                , elAttr
                                                , elAttr'
                                                , elClass
                                                , elClass'
                                                , elDynClass
                                                , elDynClass'
                                                , elementConfig_initialAttributes
                                                , inputElementConfig_elementConfig
                                                , inputElementConfig_setValue
                                                , leftmost
                                                , never
                                                , tag
                                                , text
                                                , wrapDomEvent, HasDomEvent (..), holdUniqDyn, attachWith
                                                )
import           Shared                         ( dynSimple
                                                , elLoginSignup
                                                , iFa
                                                , whenJust, elRouteLink, setRouteAndLoading, elLoading
                                                )
import           State                          ( Env(..)
                                                , Navigation(..)
                                                , Session(..)
                                                , State(..)
                                                , stageUrl
                                                , updateState, GetLoadedAndBuilt
                                                )
import           Text.Show                      ( Show(show) )
import           TextShow                       ( TextShow(showt) )
import           Type.Reflection                ( (:~~:)(HRefl)
                                                , eqTypeRep
                                                , typeRep
                                                )
import           Witherable                     ( Filterable
                                                    ( catMaybes

                                                    , mapMaybe
                                                    )
                                                )
import Data.Text (Text)
import GHCJS.DOM.Types (File)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.FileReader ( getResult, load, newFileReader, readAsText )
import Data.Semigroup (Endo)
import Data.String (String)
import qualified Palantype.Common as KI
import qualified Data.Set as Set
import qualified Palantype.Common.Stage as Stage
import qualified Data.Map.Strict as Map
import Data.Generics.Product (HasField(field))
import StenoInput (elStenoInput)

default (Text)

elFileInput :: DomBuilder t m => Text -> Event t Text -> m (Event t File)
elFileInput strClass eSet = do
    i <-
        inputElement $
            def & inputElementConfig_setValue .~ eSet
                & inputElementConfig_elementConfig
                    . elementConfig_initialAttributes
                .~ (  "type"   =: "file"
                   <> "accept" =: "text/cfg"
                   <> "class"  =: strClass
                   )

    let eFiles = _inputElement_files i
    pure $ mapMaybe listToMaybe $ updated eFiles

message ::
    forall t (m :: * -> *).
    ( DomBuilder t m
    , PostBuild t m
    , MonadHold t m
    , MonadReader (Dynamic t State) m
    , EventWriter t (Endo State) m
    , MonadFix m
    ) =>
    m ()
message = do
    dynMsg <- holdUniqDyn =<< asks (fmap $ stApp >>> stMsg)
    dyn_ $ dynMsg <&> \mMsg -> whenJust mMsg $ \Message {..} ->
      elClass "div" "overlay" $ do
        (elClose, _) <- elClass' "span" "float-right" $ iFa "fas fa-times"
        let eClose = domEvent Click elClose
        updateState $ eClose $> [field @"stApp" . field @"stMsg" .~ Nothing]
        el "div" $ text msgCaption
        el "span" $ text msgBody

elSettings
  :: forall key t (m :: * -> *)
  . ( DomBuilder t m
    , MonadHold t m
    , Palantype key
    , PostBuild t m
    , Prerender t m
    , Routed t StageIndex m
    , EventWriter t (Endo State) m
    , MonadReader (Dynamic t State) m
    , SetRoute t (R FrontendRoute) m
    , MonadFix m
    )
  => m ()
elSettings = elClass "div" "shadow-md p-1" do
    dynState <- ask
    let
        lang = if
          | Just HRefl <- typeRep @key `eqTypeRep` typeRep @DE.Key -> SystemDE
          | Just HRefl <- typeRep @key `eqTypeRep` typeRep @EN.Key -> SystemEN
          | otherwise -> $failure "Key not implemented"

    dynLoading <- holdUniqDyn $ stLoading <$> dynState
    elLoading dynLoading

    elClass "div" "float-left divide-x border-gray-500" do
        -- button to show configuration dropdown
        eFile <- elClass "div" "px-3 h-8 relative inline-block" do
            elClass "span" "group text-zinc-500 hover:text-grayishblue-800 \
                           \text-3xl cursor-pointer" do
              iFa "fas fa-cog"
              elClass "div" "group-hover:block hidden w-40 absolute bg-gray-50 \
                            \shadow-lg z-20" mdo
                  dynMCfgName <- holdUniqDyn $ fmap pcfgName
                              . view (at lang >>> _Wrapped')
                              . stPloverCfg . stApp <$> dynState
                  let
                      elCheckmark co =
                          dyn_ $ dynMCfgName <&> \cfgName ->
                              elClass "span" "inline-block w-4" $
                                  if cfgName == Just co
                                  then text "✓ "
                                  else blank

                  elClass "span" "px-4 pt-2 block text-black text-sm \
                                \font-bold border-b"
                    $ text "Keyboard layout"

                  (elQwertz, _) <- elClass' "span" "px-4 py-2 hover:bg-zinc-300 block text-base text-black" do
                      elCheckmark CNQwertzDE
                      text "qwertz DE"

                  let eQwertz = domEvent Click elQwertz
                  updateState $ eQwertz $>
                      [ field @"stApp" . field @"stPloverCfg" . _Wrapped' . ix lang
                          .~ keyMapToPloverCfg lsStenoQwertz [] "keyboard" CNQwertzDE
                      ]

                  (elQwerty, _) <- elClass' "span" "px-4 py-2 hover:bg-zinc-300 block text-base text-black" do
                      elCheckmark CNQwertyEN
                      text "qwerty EN"
                  let eQwerty = domEvent Click elQwerty
                      lsStenoQwertyEN = case lang of
                          SystemEN -> lsStenoQwertyOrig
                          _        -> lsStenoQwerty
                  updateState $
                      eQwerty
                          $> [ field @"stApp" . field @"stPloverCfg"
                                  . _Wrapped'
                                  . ix lang
                                  .~ keyMapToPloverCfg lsStenoQwertyEN [] "keyboard" CNQwertyEN
                            ]

                  eFile' <- elClass "span" "px-4 py-2 hover:bg-zinc-300 \
                                           \overflow-hidden block relative \
                                           \text-base text-black" do
                      elCheckmark CNFile
                      text "Upload plover.cfg"
                      elFileInput "opacity-0 absolute h-full top-0 left-0"
                        $ leftmost [eQwerty, eQwertz] $> ""

                  elClass "span" "px-2 pt-1 block text-black text-sm \
                                \font-bold border-b"
                    $ text "Progress"

                  (eRP, _) <- elClass' "span" "px-4 py-2 hover:bg-zinc-300 block \
                                              \text-base text-black"
                              $ text "Reset"
                  updateState $ domEvent Click eRP $>
                      [ field @"stApp" . field @"stToc" . field @"stProgress" .~ def
                      ]

                  dynSession <- holdUniqDyn $ stSession <$> dynState
                  dyn_ $ dynSession <&> \case
                    SessionAnon -> blank
                    SessionUser _ -> do
                      dynStage <- askRoute
                      elClass "span" "px-2 pt-1 block text-black text-sm \
                                    \font-bold border-b" $ text "Account"
                      (domSettings, _) <-
                        elClass' "span" "px-4 py-2 hover:bg-zinc-300 block text-base text-black"
                          $ text "Go to settings"
                      let evGotoSettings = tag (current dynStage) $ domEvent Click domSettings
                      updateState $ evGotoSettings <&> \stage ->
                        [ field @"stRedirectUrl" .~ (stageUrl @key) stage ]
                      setRouteAndLoading $ evGotoSettings $> FrontendRoute_Auth :/ AuthPage_Settings :/ ()

                  pure eFile'

        -- button to switch between palantype systems, i.e. DE and EN

        elClass "div" "px-3 h-8 relative inline-block" do
            elClass "span" "group text-zinc-500 hover:text-grayishblue-800 \
                           \text-3xl font-serif cursor-pointer" do
              text $ showSymbol lang
              elClass "div" "group-hover:block hidden w-max absolute bg-gray-50 \
                            \shadow-lg -mt-1 z-20" do
                  (eRL, _) <- elClass' "span" "px-4 py-2 hover:bg-zinc-300 block \
                                              \text-base font-sans text-black"
                              $ text "Back to system selection"
                  let eClickRL = domEvent Click eRL
                  updateState $ eClickRL $> [ field @"stApp" . field @"stMLang" .~ Nothing]
                  setRouteAndLoading $ eClickRL $> FrontendRoute_Main :/ ()

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
                           in field @"stApp" . field @"stMsg" ?~ Message {..},
                  if null pcfgUnrecognizedStenos
                      then id
                      else
                          let msgCaption = "Unrecognized steno keys"
                              msgBody =
                                  "Your key map contains unrecognized entries:\n"
                                      <> Text.intercalate
                                          "\n"
                                          (showt <$> pcfgUnrecognizedStenos)
                           in field @"stApp" . field @"stMsg" ?~ Message {..}
                ]

        -- button to toggle keyboard
        dynClass <- holdUniqDyn $ dynState <&> \st ->
                "px-3 h-8 hover:text-grayishblue-800 cursor-pointer inline-block text-2xl"
                    <> " " <> if stShow $ stKeyboard $ stApp st
                        then "text-grayishblue-800"
                        else "text-zinc-500"
        (s, _) <- elDynClass' "div" dynClass $ elClass "div" "inline-flex" do
            iFa "fas fa-keyboard"
            elClass "code" "p-1 text-xs" $ text $ showt $ KI.toRaw @key kiInsert

        updateState $ domEvent Click s $> [field @"stApp" . field @"stKeyboard" . field @"stShow" %~ not]

    dynRedirectRoute <- fmap (stageUrl @key) <$> askRoute
    elLoginSignup dynRedirectRoute

    elClass "br" "clear-both" blank

-- Table of Contents

elToc ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadFix m,
      MonadHold t m,
      Palantype key,
      Prerender t m,
      PostBuild t m,
      RouteToUrl (R FrontendRoute) m,
      SetRoute t (R FrontendRoute) m
    )
    => StageIndex
    -> Dynamic t StateToc
    -> m ()
elToc stageCurrent dynStateToc = elClass "section" "p-3 shrink-0 overflow-y-auto" do
    dynVisible <- holdUniqDyn $ stVisible <$> dynStateToc
    let dynShowStage i = Set.member i . stShowStage <$> dynStateToc
        dynClassDisplay = bool "hidden" "" <$> dynVisible
        dynClsToplevelSteno = bool "hidden" "" . stShowToplevelSteno <$> dynStateToc
        dynClsSublevelSteno = bool "hidden" "" . stShowSublevelSteno <$> dynStateToc

    -- button to toggle TOC
    elClass "div" "flex items-center" do
        dyn_ $ dynVisible <&> \bVisible -> do
            (s, _) <- if bVisible
                then
                    elAttr' "span"
                        (  "class" =: "text-zinc-400 text-2xl"
                        <> "title" =: "Hide Table of Contents"
                        ) $ iFa "fas fa-times"
                else
                    elAttr' "span"
                        (  "class" =: "text-zinc-400 text-2xl"
                        <> "title" =: "Show Table of Contents"
                        ) $ iFa "fas fa-bars"

            updateState $ domEvent Click s $> [field @"stApp" . field @"stToc" . field @"stVisible" %~ not]

        elClass "span" "steno-navigation text-xs p-1 ml-1"
          $ text $ showt $ KI.toRaw @key kiDelete

        elClass "span" "flex-grow" $ text " "

        elDynClass "span" dynClassDisplay do
          text "Go to stage "
          elClass "span" "steno-navigation text-xs p-1"
            $ text $ showt $ KI.toRaw @key KI.kiCtrlNumber

    elDynClass "div" (fmap ("mt-1 border-t " <>) dynClassDisplay) $ do

        let dynCleared = stCleared <$> dynStateToc
        dyn_ $ dynCleared <&> \cleared -> do

            let
                elLi dynClsSteno iSubstage = do
                    let cls = "whitespace-nowrap leading-7" <> " " <>
                            if iSubstage == stageCurrent
                                then "bg-zinc-200"
                                else ""
                    elClass "li" cls $ do
                        elClass "span" "w-4 inline-block px-0.5" $ if iSubstage `Set.member` cleared
                            then elClass "span" "text-green-500 text-xs" $ iFa "fas fa-check"
                            else el "span" $ text "○"
                        elRouteLink (stageUrl @key iSubstage) $ do
                            let (iSub, mg, str2) = Stage.toTOCString $
                                  case Stage.fromIndex @key iSubstage of
                                    Nothing -> $failure $ "index invalid: " <> show iSubstage
                                    Just s  -> s
                            text $ "Ex. " <> showt iSub
                            elDynClass "span"
                              ( ("steno-navigation text-xs p-1 " <>) <$> dynClsSteno)
                              $ text $ showt $ KI.toRaw @key $ $fromJust $ kiFromSmallNumber iSub
                            text ": "
                            whenJust mg \g -> el "strong" $ text $ "G" <> showt g <> " "
                            text str2

                elStage :: Int -> Text -> [StageIndex] -> m ()
                elStage i stageTitle iSubstages = do
                    (s, _) <- elClass' "li" "cursor-pointer pt-1" $ do
                        let dynClass =
                                bool "fas fa-caret-right" "fas fa-caret-down" <$> dynShowStage i
                        elClass "span" "text-grayishblue-900 text-lg w-4 inline-block"
                          $ elDynClass "i" dynClass blank
                        text $ "Stage " <> showt i
                        elDynClass "span"
                          ( ("steno-navigation text-xs p-1 " <>) <$> dynClsToplevelSteno
                          ) $ text $ showt $ KI.toRaw @key $ $fromJust $ kiFromSmallNumber i
                        text $ ": " <> stageTitle

                        let dynClassUl = bool "hidden" "" <$> dynShowStage i
                        elDynClass "ul" dynClassUl $ traverse_ (elLi dynClsSublevelSteno) iSubstages

                    let eClickS = domEvent Click s
                    updateState $ eClickS $>
                        [ field @"stApp" . field @"stToc" . field @"stShowStage" . at i %~
                            maybe (Just ()) (const Nothing)
                        ]

            el "ul" $ do

                elLi dynClsToplevelSteno 0
                elStage 1 "The Palantype Alphabet"  [1 .. 8 ]
                let lang = if
                      | Just HRefl <- typeRep @key `eqTypeRep` typeRep @EN.Key -> SystemEN
                      | Just HRefl <- typeRep @key `eqTypeRep` typeRep @DE.Key -> SystemDE
                      | otherwise -> $failure "Key not implemented"
                case lang of
                  SystemEN -> elStage 2 "Syllables and chords"           [9  .. 10]
                  SystemDE -> do
                      elStage 2  "Syllables and chords"           [9  .. 12]
                      elStage 3  "Common replacement rules"       [13 .. 15]
                      elStage 4  "Be efficient, be greedy"        [16 .. 18]
                      elStage 5  "R, L, and Diconsonants"         [19 .. 22]
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
                      elLi dynClsToplevelSteno 59

landingPage
  :: forall t (m :: * -> *)
  . ( DomBuilder t m
    , EventWriter t (Endo State) m
    , MonadReader (Dynamic t State) m
    , SetRoute t (R FrontendRoute) m
    )
  => m ()
landingPage = elClass "div" "bg-grayishblue-300" do
    elClass "div" "w-full h-28 pt-8 pl-8 text-6xl text-grayishblue-900 font-serif" $ text "Palantype DE"
    elClass "div" "bg-grayishblue-200 w-full text-center p-8" $ do
        elClass "div" "flex flex-wrap items-center justify-center" $ do
            elAttr "video"
                (  "width"    =: "480"
                <> "height"   =: "405"
                <> "autoplay" =: "autoplay"
                <> "muted"    =: "muted"
                <> "loop"     =: "loop"
                <> "preload"  =: "auto"
                <> "controls"  =: "controls"
                <> "class" =: "bg-white m-1 border-8 border-white border-solid"
                ) $ do
                elAttr "source" ("src" =: $(static "palantype-short.mp4") <> "type" =: "video/mp4") blank
                text "Your missing out on a great video here ¯\\_(ツ)_/¯"
            elClass "div" "flex-col justify-around" $ do
                elClass "h1" "text-white text-6xl py-16" $ text "Type as fast as you speak"
                elClass "div" "flex flex-wrap justify-center" $ do
                    (elDE, _) <- elClass "div" "mx-8" $ do
                        elClass' "button" "text-white bg-grayishblue-800 h-[198px] w-[360px] p-1 rounded-3xl cursor-pointer" $ do
                            elClass "div" "flex items-center m-1 p-1" $ do
                                elClass "div" "flex-grow shrink-0" $ do
                                    let elFlag cc =
                                          elClass "span" ("m-0.5 flag-icon flag-icon-squared flag-icon-" <> cc) blank
                                    elFlag "de"
                                    el "br" blank
                                    elFlag "at"
                                    elFlag "ch"
                                elClass "div" "m-2 flex-grow shrink-0 text-xl font-bold" $ text "DE"
                                elClass "div" "m-1 p-1 grow-0 shrink-1" $
                                    text
                                        "35 interactive tutorials. 2 Million words and growing. A steno system designed for \
                                        \the German language."
                            elClass "div" "m-1 text-5xl font-bold" $ text "Start"

                    elEN <- elClass "div" "other" $ do
                        (elEN, _) <- elClass' "button" "h-[100px] w-[296px] m-1 bg-grayishblue-500 rounded-3xl cursor-pointer"$
                            elClass "div" "flex items-center m-1" $ do
                                elClass "div" "flex-grow shrink-0 m-1" $
                                    elAttr "img" ("src" =: $(static "palantype.png")) blank
                                elClass "div" "flex-grow shrink-0 text-xl font-bold" $ text "EN"
                                elClass "div" "grow-0 shrink p-2 text-white" $
                                    text
                                        "The original palantype steno system for English, \
                                        \brought to your keyboard."

                        elClass "div" "p-1 h-[90px] w-[296px] m-1 items-center \
                                      \rounded-3xl text-grayishblue-900 flex \
                                      \border-solid border border-black"
                          $ el "div" $ do
                            text "Missing a language? Checkout the "
                            elAttr "a"
                              (  "href" =: "https://github.com/rubenmoor/palantype-tools"
                              ) $ text "source on Github"
                            text " to create your own Palantype-style steno system."
                        pure elEN

                    let evClick = leftmost
                          [ domEvent Click elEN $> (SystemEN, $fromJust $ mkStageIndex @EN.Key 0)
                          , domEvent Click elDE $> (SystemDE, $fromJust $ mkStageIndex @DE.Key 0)
                          ]
                    updateState $
                        evClick <&> \(lang, si0) ->
                            [ field @"stApp" . field @"stMLang" ?~ lang
                              -- if no progress in map, insert "Introduction"
                            , field @"stApp" . field @"stToc" . field @"stProgress"
                                %~ Map.insertWith (\_ o -> o) lang si0
                            ]
                    dynState <- ask
                    let
                        behMapProgress = stProgress . stToc . stApp <$> current dynState
                        selectRoute m (lang, si0) =
                          let stage = Map.findWithDefault si0 lang m
                          in  case lang of
                                SystemEN -> stageUrl @EN.Key stage
                                SystemDE -> stageUrl @DE.Key stage
                    setRouteAndLoading $ attachWith selectRoute behMapProgress evClick

    elClass "div" "flex flex-wrap justify-center py-8" $ do

        let twUsp = "max-w-xs px-8 my-8 text-gray-200"
            twCaption = "text-xl font-bold italic border-b border-solid border-white mb-4"
        elClass "div" twUsp do
            elClass "div" "h-32 text-center text-8xl text-grayishblue-900" $ iFa "fas fa-rocket"
            elClass "div" twCaption $ text "Maximum typing speed"
            elClass "div" "text-lg" $
                text
                    "Reach a typing speed of up to 300 \
                    \words per minute, fast enough to type along as people talk."

        elClass "div" twUsp do
            elClass "div" "h-32 text-center" $ elAttr "img"
                 (  "src"    =: $(static "chords.gif")
                 <> "width"  =: "128"
                 <> "height" =: "128"
                 <> "class"  =: "inline"
                 ) blank
            elClass "div" twCaption $ text "Type chords, not letters"
            elClass "div" "text-lg" $
                text
                    "Input whole words or word parts with a single stroke \
                    \using multiple fingers at once. This is why it's so fast \
                    \and why it requires a lot of practice."

        elClass "div" twUsp $ do
            elClass "div" "h-32 text-center" $ elAttr "img"
                 (  "src"    =: $(static "keyboard-icon.png")
                 <> "width"  =: "128"
                 <> "height" =: "128"
                 <> "class"  =: "inline"
                 ) blank
            elClass "div" twCaption $ text "No special hardware"
            elClass "div" "text-lg" $ do
                text "You will need a keyboard that supports "
                elAttr "a" (  "href" =: "https://en.wikipedia.org/wiki/Rollover_(keyboard)"
                           ) $ text "N-key roll-over"
                text ", to register all the keys that you press simultaneously, and \
                     \optionally an ortho-linear key layout."

        elClass "div" twUsp $ do
            elClass "div" "h-32 text-center" $ elAttr "img"
                 (  "src"    =: $(static "opensource-icon.png")
                 <> "width"  =: "100"
                 <> "height" =: "100"
                 <> "class"  =: "inline"
                 ) blank
            elClass "div" twCaption $ text "Free and open-source"
            elClass "div" "text-lg" $ do
                text "Find the code on Github and contribute by reporting bugs \
                     \and requesting features in the "
                elAttr "a" (  "href"  =: "https://github.com/rubenmoor/learn-palantype/issues"
                           ) $ text "issue tracker"
                text "."

    elClass "div" "py-8 text-grayishblue-900 text-2xl text-center bg-gray-200" $ do
        text "Want to reach out? Join the "
        elAttr "a"
          (  "href"  =: "https://discord.gg/spymr5aCr5"
          ) $ text "Plover Discord Server"
        text " and find me in #palantype, @gurubm."

    elClass "div" "text-center p-8" $ do
      elClass "h1" "text-6xl text-grayishblue-900 py-4" $ text "The technology"

      elClass "dl" "text-xl text-gray-200" do
        let twTerm = "font-bold pt-6"
            twDefinition = "pt-3"
        elClass "dt" twTerm $ text "Obsidian Systems Obelisk"
        elClass "dd" twDefinition do
          text "Functional reactive programming for web and mobile—a sublime experience, find the "
          elAttr "a"
            (  "href" =: "https://github.com/obsidiansystems/obelisk"
            ) $ text "code on GitHub"
          text "."
        elClass "dt" twTerm $ text "GHCJS"
        elClass "dd" twDefinition do
          text "Let the JavaScript be generated and stay type-safe and functional all the way, cf. "
          elAttr "a"
            (  "href" =: "https://github.com/ghcjs/ghcjs"
            ) $ text "GHCJS on GitHub"
          text "."
        elClass "dt" twTerm $ text "Haskell"
        elClass "dd" twDefinition do
          text "Category theory, lazy evaluation, purely functional programming since 1990. "
          elAttr "a"
            (  "href" =: "https://en.wikipedia.org/wiki/Haskell"
            ) $ text "Read more on Wikipedia"
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
       , SetRoute t (R FrontendRoute) (Client m)
       , TriggerEvent t m
       )
    => GetLoadedAndBuilt t
    -> RoutedT t StageIndex
         (ReaderT (Dynamic t State)
           ( EventWriterT t (Endo State) m
           )
         ) ()
elStages getLoadedAndBuilt = mdo
    elClass "header" "h-[47px] z-10" $ elSettings @key
    dynCurrent <- askRoute
    dyn_ $ dynCurrent <&> stages'
  where
    mkNavigation si =
      let navMPrevious = Stage.mPrev si
          navCurrent = si
          navMNext = Stage.mNext @key si
          navPageName = maybe "stageindex-unknown" Stage.toPageName
            $ Stage.fromIndex @key si
          navTextLang = TextEN
          navSystemLang = getSystemLang @key
      in  Navigation{..}

    stages' ::
        StageIndex -> RoutedT t StageIndex (ReaderT (Dynamic t State) (EventWriterT t (Endo State) m)) ()
    stages' iCurrent = elClass "div" "py-1 flex flex-col flex-nowrap h-[calc(100%-47px)]" do

        dynState' <- ask

        let navigation = mkNavigation iCurrent
        dynActive <- holdUniqDyn $ stActive . stKeyboard . stApp <$> dynState'

        evMChord <- dynSimple $ dynActive <&> \case
          True  -> do
            dynStateKeyboard <- holdUniqDyn $ stKeyboard . stApp <$> dynState'
            dynPloverCfg <- holdUniqDyn $ stPloverCfg . stApp <$> dynState'
            dynSimple $ dynPloverCfg <&> \ploverCfg ->
              elClass "div" "mx-auto border-b border-dotted"
              $ postRender $ elStenoInput @key dynStateKeyboard ploverCfg navigation getLoadedAndBuilt
          False -> do
            elClass "div" "p-2 text-center mx-auto text-gray-500 border border-solid border-gray-200 \
                          \rounded-lg" do
                text "The interactive keyboard is deactivated"
                (elPowerOn, _) <- elAttr' "span"
                    ( "class" =: "pl-2 text-green-700 cursor-pointer"
                    <> "title" =: "Switch on interactive input"
                    ) $ iFa "fas fa-power-off"
                updateState $ domEvent Click elPowerOn $>
                  [ field @"stApp" . field @"stKeyboard" . field @"stActive" .~ True ]
            pure never

        elClass "div" "flex flex-grow flex-row flex-nowrap overflow-y-hidden" mdo
            dynTocState <- holdUniqDyn $ stToc . stApp <$> dynState'
            elToc @key iCurrent dynTocState

            let setEnv page = mapRoutedT
                  ( withReaderT $ \dynState -> Env
                      { envDynState = dynState
                      , envEvMChord = evMChord
                      , envNavigation = navigation
                      , envGetLoadedAndBuilt = getLoadedAndBuilt
                      }
                  ) do
                      dynRoute <- askRoute
                      Env{..} <- ask
                      evLoadedAndBuilt <- envGetLoadedAndBuilt
                      dynAuthData <- holdUniqDyn $ getMaybeAuthData <$> envDynState
                      void $ request $ postEventViewPage
                        dynAuthData
                        (Right . showRoute . stageUrl @key <$> dynRoute)
                        evLoadedAndBuilt
                      page
            elAttr "section" (  "class" =: "overflow-y-auto scroll-smooth px-2 \
                                           \w-full h-full relative"
                             <> "id" =: "content"
                             ) do
                elClass "div" "w-max sticky top-[2px] text-xs p-1 steno-navigation mx-auto \
                              \opacity-50" $ text
                    $  "⯅ " <> showt (KI.toRaw @key kiUp)
                    <> "  ↟ " <> showt (KI.toRaw @key kiPageUp)
                elClass "div" "before:content-[\"\"] before:w-4/5 before:bg-white \
                              \before:absolute before:top-0 before:h-[31px] \
                              \after:content-[\"\"] after:w-4/5 after:bg-white \
                              \after:absolute after:h-[31px] after:z-10 \
                              \after:h-[320px] \
                              \p-4"
                  $ setEnv do
                    let
                        elPageNotImplemented str = do
                          elClass "div" "text-xs text-grayishblue-900" do
                            el "p" $ text ("Page not implemented: StageIndex " <> showt iCurrent)
                            el "p" $ text str

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
                          elPageNotImplemented "special page not found"
                      Just (Stage (StageGeneric pg g) _) -> getGenericExercise pg g
                      Nothing -> elPageNotImplemented "index invalid"

                elClass "div" "w-max sticky bottom-4 text-xs p-1 steno-navigation mx-auto opacity-50"
                  $ text $ "⯆ "
                    <> showt (KI.toRaw @key kiDown)
                    <> "  ↡ "
                    <> showt (KI.toRaw @key kiPageDown)
        elFooter @key navigation
