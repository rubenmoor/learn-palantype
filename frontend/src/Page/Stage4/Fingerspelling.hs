{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Page.Stage4.Fingerspelling
    ( fingerspelling
    ) where

import Common.Route (FrontendRoute)
import Control.Applicative (Applicative (pure))
import Control.Monad (when, unless)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader.Class (MonadReader, ask)
import Data.Bool (Bool (..))
import Data.Foldable (for_)
import Data.Function (($))
import Data.Functor ((<&>))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List ((!!), zip)
import Data.Maybe (Maybe (..))
import Data.Semigroup (Endo, (<>))
import Obelisk.Route.Frontend (R, RouteToUrl, SetRoute, routeLink)
import Page.Common (elCongraz, elNotImplemented)
import Palantype.Common (kiBackUp, Chord, Palantype)
import Reflex.Dom (EventWriter, holdUniqDyn, updated, Event, (=:), DomBuilder, MonadHold, PostBuild, Prerender, blank, dynText, dyn_, el, elAttr, elClass, elDynClass, foldDyn, text, widgetHold_)
import State (State, Env (..), Navigation (..), stageUrl)
import TextShow (TextShow (showt))
import Text.Read (readMaybe)
import Text.Show (Show(show))
import Palantype.Common.TH (readLoc)
import Control.Category ((<<<))
import Common.Stage (stageMeta)
import Palantype.DE.FingerSpelling (dictLiterals, keysLetterOther, keysLetterUS)
import qualified Data.Text as Text
import Palantype.Common (Lang(DE))
import Data.Eq (Eq((==)))
import GHC.Num ((+), Num((-)))
import Data.Tuple (fst)
import qualified Palantype.Common.RawSteno as Raw
import qualified Palantype.Common.Indices as KI
import Data.Foldable (Foldable(length))
import Data.Ord (Ord((>)))
import Data.Functor (void)
import Data.Witherable (Filterable(filter))
import Control.Category (Category(id))

fingerspelling
  :: forall key t (m :: * -> *)
  . ( DomBuilder t m
    , EventWriter t (Endo State) m
    , MonadFix m
    , MonadHold t m
    , MonadReader (Env t key) m
    , Palantype key
    , PostBuild t m
    , Prerender t m
    , RouteToUrl (R FrontendRoute) m
    , SetRoute t (R FrontendRoute) m
    )
  => m Navigation
fingerspelling = do
    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang == DE) elNotImplemented

    el "h1" $ text "Fingerspelling"

    el "h2" $ text "Palantype fingerspelling mode"

    elClass "div" "paragraph" $
        text "Whenever a word does not exist in the installed dictionary, \
             \you won't be able to type it according to the rules presented here. \
             \Of course, the idea is that efficient steno chords exist for \
             \any conceivable situation, however ..."

    elClass "div" "paragraph" $ do
        text "There are"
        el "ul" $ do
            el "li" $ text "very rare words, not yet accounted for by the \
                           \algorithm,"
            el "li" $ text "loanwords, partially accounted for by means of \
                           \explicit exceptions,"
            el "li" $ text "abbreviations and symbols, partially accounted for,"
            el "li" $ text "and diverging ways on how to write things."

    elClass "div" "paragraph" $ do
        text "This goes to say: If a word doesn't come out right, there is \
             \a troubleshooting procedure. Maybe there is a proper steno \
             \spelling, you just don't know it? In that case, you might \
             \want to look up the word within the dictionary, using Plover. \
             \If, however, there \
             \is no steno code for whatever you want to write, you have a \
             \general alternative that is called fingerspelling. \
             \I.e. there are steno codes for every single letter (and digit, \
             \special character, and command key) and, this way, it is possible to \
             \type sequentially, letter by letter, using steno."

    el "h3" $ text "Standard letters"

    elClass "div" "paragraph" $ do
        text "The Standard Letters are simply all those letters that are present \
             \on the US keyboard layout. They have special significance, because \
             \they commonly feature in shortcuts, like "
        el "code" $ text "CTRL+C"
        text " or "
        el "code" $ text "CTRL+-"
        text " (to zoom out in your internet browser). And indeed, the palantype \
             \fingerspelling mode accounts for that by providing modifier keys: "
        el "code" $ text "CTRL"
        text ", "
        el "code" $ text "WIN"
        text ", "
        el "code" $ text "ALT"
        text ", and "
        el "code" $ text "SHIFT"
        text ". Note that "
        el "code" $ text "SHIFT"
        text " is a secondary modifier and can be added on top of the other \
             \modifiers. This is why "
        el "code" $ text "SHIFT"
        text " has a finger apart from them."

    elClass "div" "paragraph" $ do
        text "Be aware that "
        el "code" $ text "WIN"
        text ", a key whose actual name is "
        el "code" $ text "SUPER"
        text ", appears strictly as modifier here. In case you want to tap the "
        el "code" $ text "WIN"
        text " key, e.g. to open the Start menu, the "
        el "code" $ text "WIN"
        text " key shows up among the keys in "
        let stageCommandKeys = $readLoc "stage_commandKeys"
        routeLink (stageUrl navLang stageCommandKeys) $
            text $ showt $ stageMeta stageCommandKeys
        text "."

    elClass "div" "paragraph" $ do
        text "Use "
        el "code" $ text "-L"
        text " in combination with the keys below, to start fingerspelling!"

    el "h4" $ text "Modifier keys"

    elClass "div" "patternTable" $ do
        elClass "div" "floatLeft" $ do
            elAttr "div" ("class" =: "orig"  ) $ text "SHIFT"
            elAttr "code" ("class" =: "steno") $ text "-S"
        elClass "div" "floatLeft" $ do
            elAttr "div" ("class" =: "orig"  ) $ text "CTRL"
            elAttr "code" ("class" =: "steno") $ text "s"
        elClass "div" "floatLeft" $ do
            elAttr "div" ("class" =: "orig"  ) $ text "WIN"
            elAttr "code" ("class" =: "steno") $ text "D"
        elClass "div" "floatLeft" $ do
            elAttr "div" ("class" =: "orig"  ) $ text "ALT"
            elAttr "code" ("class" =: "steno") $ text "n"
        elClass "br" "clearBoth" blank

    el "h4" $ text "Fingerspelling literals"

    elClass "div" "patternTable" $ do
        for_ keysLetterUS \(c, steno) ->
            elClass "div" "floatLeft" $ do
                elAttr "div" ("class" =: "orig" ) $ text $ Text.singleton c
                elAttr "code" ("class" =: "steno") $ text steno
        elClass "br" "clearBoth" blank

    el "h3" $ text "Non-standard letters"

    elClass "div" "paragraph" $ do
        text "There is nothing special about the following letters. Only, \
             \they can be combined with SHIFT for their uppercase version \
             \and nothing else."

    elClass "div" "patternTable" $ do
        for_ keysLetterOther \(c, steno) ->
            elClass "div" "floatLeft" $ do
                elAttr "div" ("class" =: "orig" ) $ text $ Text.singleton c
                elAttr "code" ("class" =: "steno") $ text steno
        elClass "br" "clearBoth" blank

    elClass "div" "paragraph" $
        text "Fingerspell the letters as they appear!"

    eDone <- taskLiterals
    elCongraz eDone envNavigation

    elClass "div" "paragraph" $ do
        text "Fingerspelling is a powerfull feature. Together with "
        let stageCommandKeys = $readLoc "stage_commandKeys"
        routeLink (stageUrl navLang stageCommandKeys) $
            text $ showt $ stageMeta stageCommandKeys
        text " you can utilize any conventional key binding in steno mode \
             \without any additional configuration."

    pure envNavigation

data WalkLiteralsState k = WalkLiteralsState
    { wlsCounter  :: Int
    , wlsMMistake :: Maybe (Int, Chord k)
    , wlsDone     :: Bool
    }

{-|
Pass through all the letters of the steno alphabet one by one
-}
taskLiterals
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       )
    => m (Event t ())
taskLiterals = do
    Env {..} <- ask
    let Navigation {..} = envNavigation

        len = length dictLiterals

        step :: Chord key -> WalkLiteralsState key -> WalkLiteralsState key
        step c wls@WalkLiteralsState {..} =
            let current = KI.toRaw @key (fst (dictLiterals !! wlsCounter))
            in  case (wlsMMistake, wlsDone) of

                  -- reset after done
                  (_, True) ->
                      wls { wlsDone = False, wlsCounter = 0 }

                  -- back up after mistake
                  (Just _, _) | Raw.fromChord c == KI.toRaw @key kiBackUp ->
                      wls { wlsMMistake = Nothing }

                  -- ... or stay in mistake mode
                  (Just _, _) -> wls

                  -- correct
                  _ | current == Raw.fromChord c -> wls
                      { wlsDone    = if wlsCounter == len - 1
                                        then True
                                        else False
                      , wlsCounter = wlsCounter + 1
                      }

                  -- mistake
                  _ -> wls
                      { wlsDone     = False
                      , wlsMMistake = Just (wlsCounter, c)
                      }

        stepInitial = WalkLiteralsState
            { wlsCounter  = 0
            , wlsMMistake = Nothing
            , wlsDone     = False
            }

    dynWalk <- foldDyn step stepInitial envEChord
    dynDone <- holdUniqDyn $ wlsDone <$> dynWalk
    let eDone = updated dynDone

    elClass "div" "exerciseField multiline" $ el "code" $ do
        for_ (zip [0 :: Int ..] dictLiterals) $ \(i, (_, lit)) -> do
            let
                dynCls = dynWalk <&> \WalkLiteralsState {..} -> case wlsMMistake of
                    Just (j, _) -> if i == j         then "bgRed"   else ""
                    Nothing     -> if wlsCounter > i then "bgGreen" else ""
            elDynClass "span" dynCls $ text lit

    elClass "div" "paragraph" $ do
        dynText $ dynWalk <&> \WalkLiteralsState {..} -> Text.pack $ show wlsCounter
        text $ " / " <> Text.pack (show len)

    let eMistake = wlsMMistake <$> updated dynWalk
    widgetHold_ blank $ eMistake <&> \case
        Just (_, w) -> do
            elClass "div" "red small paragraph" $ do
                text $ "You typed " <> showt w <> " "
                elClass "span" "btnSteno blinking" $
                    text $ "↤ " <> showt (KI.toRaw @key kiBackUp) -- U+21A4

        Nothing -> blank

    dyn_ $ dynDone <&> \bDone ->
        when bDone $ elClass "div" "small anthrazit" $ text
            "Cleared. Press any key to start over."

    pure $ void $ filter id eDone
