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

module Page.Stage4.PloverCommands
    ( ploverCommands
    ) where

import Client (getDictDENumbers, getDocDEPatternAll, postRender, request)
import Common.Route (FrontendRoute)
import Control.Applicative (Applicative (pure))
import Control.Monad ((=<<))
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader.Class (MonadReader, ask)
import Data.Bool (Bool (..), bool, not)
import Data.Either (Either (..))
import Data.Foldable (for_)
import Data.Function (($))
import Data.Functor ((<&>))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (zip)
import qualified Data.Map.Strict as Map
import Data.Maybe (Maybe (..))
import Data.Semigroup ((<>))
import Obelisk.Route.Frontend (R, RouteToUrl, SetRoute, routeLink)
import Page.Common (elPatterns, loading)
import Palantype.Common (toDescription)
import Reflex.Dom ((=:), DomBuilder, EventName (Click), MonadHold, PostBuild, Prerender, blank, delay, domEvent, dynText, dyn_, el, elAttr, elClass, elClass', elDynClass, foldDyn, getPostBuild, text, widgetHold_)
import Shared (iFa)
import State (Env (..), Navigation (..), stageUrl)
import TextShow (TextShow (showt))
import Text.Read (readMaybe)
import Text.Show (Show(show))
import Obelisk.Generated.Static (static)

ploverCommands ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      PostBuild t m,
      Prerender t m,
      RouteToUrl (R FrontendRoute) m,
      SetRoute t (R FrontendRoute) m
    ) =>
    m Navigation
ploverCommands = do
    Env {..} <- ask
    let Navigation {..} = envNavigation

    el "h1" $ text "Formatting input"

    el "h2" $ text "On our way to actual typing"

    elClass "div" "paragraph" $
        text "Learning to type individual words is fun and all, \
             \but so far we are still missing the basics of formatting. \
             \The exercises of Chapter 4 are all about that. \
             \Let's close some gaps to get closer to productive use of \
             \our palantype-style steno skills."

    el "h3" $ text "Punctuation"

    elClass "div" "paragraph" $
        text "The list below is long. Luckily you won't have to remember \
             \all of the commands. Rather, learn them as you feel the necessity."

    elClass "div" "paragraph" $ do
        text "Most importantly, use "
        el "code" $ text "A"
        text " for comma, "
        el "code" $ text "N-"
        text " for full stop, and "
        el "code" $ text "B-"
        text " to capitalize last word retroactively. This last one is important \
             \because Plover will try to handle capitalization for you. This \
             \means the first word of each sentence will be capitalized and \
             \usually nouns will be. However, whenever a german word does not \
             \solely exists as noun, the system will output the word \
             \uncapitalized and leave the decision to you."

    -- common orthography
    el "table" $ do
        el "tr" $ do
            el "th" $ text "Short"
            el "th" $ text "Key"
            el "th" $ text "Description"
            el "th" $ text "Plover code"
        el "tr" $ do
            el "td" $ text ","
            el "td" $ el "code" $ text "A"
            el "td" $ text "attach comma"
            el "td" $ el "code" $ text "{^,}"
        el "tr" $ do
            el "td" $ text ";"
            el "td" $ el "code" $ text "NA"
            el "td" $ text "attach semicolon"
            el "td" $ el "code" $ text "{^;}"
        el "tr" $ do
            el "td" $ text "-"
            el "td" $ el "code" $ text "~"
            el "td" $ text "hyphen to attach words"
            el "td" $ el "code" $ text "{^-^}"
        el "tr" $ do
            el "td" $ text "\\t"
            el "td" $ el "code" $ text "DJ"
            el "td" $ text "tab like t"
            el "td" $ el "code" $ text "{^\\t^}"
        el "tr" $ do
            el "td" $ text " "
            el "td" $ el "code" $ text "B-"
            el "td" $ text "capitalize last word"
            el "td" $ el "code" $ text "{*-|}"
        el "tr" $ do
            el "td" $ text " "
            el "td" $ el "code" $ text "D-"
            el "td" $ text "capitalize next word"
            el "td" $ el "code" $ text "{-|}"
        el "tr" $ do
            el "td" $ text " "
            el "td" $ el "code" $ text "S-"
            el "td" $ text "uncapitalize last word"
            el "td" $ el "code" $ text "{*>}"
        el "tr" $ do
            el "td" $ text "␣"
            el "td" $ el "code" $ text "G-"
            el "td" $ text "retroactively add space"
            el "td" $ el "code" $ text "{*?}"
        el "tr" $ do
            el "td" $ text "\n"
            el "td" $ el "code" $ text "J"
            el "td" $ text "paragraph: period, two newlines"
            el "td" $ el "code" $ text "{^.\n\n^}{-|}"
        el "tr" $ do
            el "td" $ text " "
            el "td" $ el "code" $ text "F-"
            el "td" $ text "retroactively delete space"
            el "td" $ el "code" $ text "{*!}"
        el "tr" $ do
            el "td" $ text "."
            el "td" $ el "code" $ text "N-"
            el "td" $ text "full stop: period"
            el "td" $ el "code" $ text "{^.}{-|}"
        el "tr" $ do
            el "td" $ text ":"
            el "td" $ el "code" $ text "L-"
            el "td" $ text "attach colon"
            el "td" $ el "code" $ text "{^:}"
        el "tr" $ do
            el "td" $ text ":"
            el "td" $ el "code" $ text "JL-"
            el "td" $ text "colon and capitalize"
            el "td" $ el "code" $ text "{^:}{-|}"
        el "tr" $ do
            el "td" $ text "?"
            el "td" $ el "code" $ text "JN-"
            el "td" $ text "question mark and capitalize"
            el "td" $ el "code" $ text "{^?}{-|}"
        el "tr" $ do
            el "td" $ text "!"
            el "td" $ el "code" $ text "JR"
            el "td" $ text "exclamation mark and capitalize"
            el "td" $ el "code" $ text "{^!}{-|}"
        el "tr" $ do
            el "td" $ text "#"
            el "td" $ el "code" $ text "H"
            el "td" $ text "hash with next word attached"
            el "td" $ el "code" $ text "{\\#^}"
        el "tr" $ do
            el "td" $ text "§"
            el "td" $ el "code" $ text "BD-"
            el "td" $ text "legal paragraph symbol"
            el "td" $ el "code" $ text "§"
        el "tr" $ do
            el "td" $ text "°"
            el "td" $ el "code" $ text "GD-"
            el "td" $ text "attach degree symbol"
            el "td" $ el "code" $ text "{^°}"
        el "tr" $ do
            el "td" $ text "™"
            el "td" $ el "code" $ text "DM-"
            el "td" $ text "attach trademark symbol"
            el "td" $ el "code" $ text "{^™}"
        el "tr" $ do
            el "td" $ text "©"
            el "td" $ el "code" $ text "GDM-"
            el "td" $ text "attach copyright symbol"
            el "td" $ el "code" $ text "{^©}"
        el "tr" $ do
            el "td" $ text "€"
            el "td" $ el "code" $ text "E"
            el "td" $ text "euro symbol"
            el "td" $ el "code" $ text "€"
        el "tr" $ do
            el "td" $ text "—"
            el "td" $ el "code" $ text "~Ü"
            el "td" $ text "em dash"
            el "td" $ el "code" $ text "—"

    -- parentheses
    el "h3" $ text "Opening and closing"

    elClass "div" "paragraph" $
        text "The commands that follow adhere to a strict logic \
             \and thus aren't that scary. Your fingers of your right \
             \hand take care of any thing that opens and closes."

    el "table" $ do
        el "tr" $ do
            el "th" $ text "Short"
            el "th" $ text "Description"
            el "th" $ text "Key 1"
            el "th" $ text "Key 2"
            el "th" $ text "Plover code"
        el "tr" $ do
            el "td" $ text "«»"
            el "td" $ text "Guillemets"
            el "td" $ el "code" $ text "+"
            el "td" $ el "code" $ text "-G"
            el "td" $ do
                el "code" $ text "{«^}"
                text ","
                el "code" $ text "{^»}"
        el "tr" $ do
            el "td" $ text "„“"
            el "td" $ text "german quotes"
            el "td" $ el "code" $ text "-L"
            el "td" $ el "code" $ text "-N"
            el "td" $ do
                el "code" $ text "{„^}"
                text ","
                el "code" $ text "{^“}"
        el "tr" $ do
            el "td" $ text "‹›"
            el "td" $ text "chevrons"
            el "td" $ el "code" $ text "-M"
            el "td" $ el "code" $ text "-B"
            el "td" $ do
                el "code" $ text "{‹^}"
                text ","
                el "code" $ text "{^›}"
        el "tr" $ do
            el "td" $ text "[]"
            el "td" $ text "square brackets"
            el "td" $ el "code" $ text "-F"
            el "td" $ el "code" $ text "s"
            el "td" $ do
                el "code" $ text "{[^}"
                text ","
                el "code" $ text "{^]}"
        el "tr" $ do
            el "td" $ text "()"
            el "td" $ text "parenthesis"
            el "td" $ el "code" $ text "-S"
            el "td" $ el "code" $ text "-D"
            el "td" $ do
                el "code" $ text "{(^}"
                text ","
                el "code" $ text "{^)}"
        el "tr" $ do
            el "td" $ text "{}"
            el "td" $ text "brackets"
            el "td" $ el "code" $ text "ʃ"
            el "td" $ el "code" $ text "n"
            el "td" $ do
                el "code" $ text "{\\{^}"
                text ","
                el "code" $ text "{^\\}}"

    -- ascii smileys
    el "h3" $ text "ASCII art"

    elClass "div" "paragraph" $
        text "Steno typing lends itself well to all kinds of makros, \
             \among them I included my personal selection of ASCII \
             \emoticons."

    el "table" $ do
        el "tr" $ do
            el "td" $ el "code" $ text "SLNSD"
            el "td" $ el "code" $ text "¯\\_(ツ)_/¯"
        el "tr" $ do
            el "td" $ el "code" $ text "BLNSD"
            el "td" $ el "code" $ text "ʕ•ᴥ•ʔ"
        el "tr" $ do
            el "td" $ el "code" $ text "GLNSD"
            el "td" $ el "code" $ text "(´･_･`)"
        el "tr" $ do
            el "td" $ el "code" $ text "HLNSD"
            el "td" $ el "code" $ text "(⊃｡•́‿•̀｡)⊃"
        el "tr" $ do
            el "td" $ el "code" $ text "DLNSD"
            el "td" $ el "code" $ text "(╯°□°）╯︵ ┻━┻"
        el "tr" $ do
            el "td" $ el "code" $ text "FLNSD"
            el "td" $ el "code" $ text "(☞ﾟヮﾟ)☞"
        el "tr" $ do
            el "td" $ el "code" $ text "MLNSD"
            el "td" $ el "code" $ text "(๑•́ ₃ •̀๑)"
        el "tr" $ do
            el "td" $ el "code" $ text "JLNSD"
            el "td" $ el "code" $ text "┬─┬⃰͡ (ᵔᵕᵔ͜ )"
        el "tr" $ do
            el "td" $ el "code" $ text "WLNSD"
            el "td" $ el "code" $ text "( ˘ ³˘)♥"
        el "tr" $ do
            el "td" $ el "code" $ text "LLNSD"
            el "td" $ el "code" $ text "( ͡° ͜ʖ ͡°)"
        el "tr" $ do
            el "td" $ el "code" $ text "NLNSD"
            el "td" $ el "code" $ text "( ಠ ʖ̯ ಠ )"
        el "tr" $ do
            el "td" $ el "code" $ text "RLNSD"
            el "td" $ el "code" $ text "(ᵔᴥᵔ)"

    -- -- plover
    -- , ("=undo"                   , "ILNSD" ) -- undo last input
    -- , ("{PLOVER:TOGGLE}"         , "BDJN+D")
    -- , ("{PLOVER:ADD_TRANSLATION}", "BDJNA" )
    -- , ("{PLOVER:LOOKUP}"         , "BDJNL" ) -- plover search dialogue
    -- , ("{PLOVER:SUGGESTIONS}"    , "BDJNS" ) -- plover suggestions window
    -- , ("{PLOVER:FOCUS}"          , "BDJNF" ) -- focus plvoer main window
    -- , ("{PLOVER:CONFIGURE}"      , "BDJNG" ) -- plover configuration window
    -- , ("{PLOVER:CONFIGURE}"      , "BDJN+G") -- quit plover
    -- ]

    evPb <- postRender $ delay 0.1 =<< getPostBuild
    evEDict <- request $ getDictDENumbers evPb

    el "h2" $ text "Palantype number mode"

    el "h3" $ text "Digits and related symbols"

    elClass "div" "paragraph" $ do
        text "For typing numbers, the virtual keyboard above can assist \
             \you quite a bit. Just hold down "
        el "code" $ text "WN-"
        text " and you can see, how to reach numbers and related symbols."

    elClass "div" "paragraph" $
        elAttr "img" (  "src" =: $(static "numbermode.png")
                     <> "alt" =: "Keyboard layout in number mode"
                     ) blank

    elClass "div" "paragraph" $ do
        text "Note how, apart from the digits 0-9 for the fingers of your \
             \right hand, the extra keys for the thumbs allow to input \
             \even longer numbers all at once, in particular common dates \
             \like "
        el "em" $ text "1990"
        text ", or "
        el "em" $ text "2022"
        text "."

    elClass "div" "paragraph" $ do
        text "Also, the input of common shortcuts that involve numbers \
             \is possible by adding a modifier key to any input. \
             \The available modifiers are "
        el "code" $ text "Control"
        text ", "
        el "code" $ text "Super"
        text ", and "
        el "code" $ text "Alt"
        text ". "
        el "code" $ text "Super"
        text " is usually called the Windows-key."

    el "h3" $ text "Special characters"

    elClass "div" "paragraph" $
        text "Following the standard US keyboard layout, you can reach \
             \special characters using the Shift modifier key with numbers. \
             \The virtual keyboard assists you here again."

    elClass "div" "paragraph" $
        elAttr "img" (  "src" =: $(static "numbermode.png")
                     <> "alt" =: "Keyboard layout in number mode"
                     ) blank

    elClass "div" "paragraph" $
        text "Note that access to these special chars via the number mode \
             \shouldn't be usually necessary."

    -- TODO: link to further special chars
    -- TODO: link to plover commands

    widgetHold_ loading $ evEDict <&> \case
        Left  str -> elClass "span" "red small" $ text $ "Couldn't load resource: " <> str
        Right map -> blank

    pure envNavigation
