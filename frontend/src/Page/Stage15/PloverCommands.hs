{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Page.Stage15.PloverCommands
    ( ploverCommands
    ) where

import Control.Applicative (Applicative (pure))
import Control.Monad (unless)
import Control.Monad.Reader.Class (MonadReader, ask)
import Data.Eq (Eq ((==)))
import Data.Function (($))
import Data.Semigroup ((<>))
import Page.Common (elNotImplemented)
import Palantype.Common (SystemLang(SystemDE))
import Reflex.Dom ((=:), DomBuilder, el, elAttr, elClass, text)
import State (Env (..), Navigation (..))

ploverCommands ::
    forall key t (m :: * -> *).
    ( DomBuilder t m
    , MonadReader (Env t key) m
      -- RouteToUrl (R FrontendRoute) m,
      -- SetRoute t (R FrontendRoute) m
    ) =>
    m Navigation
ploverCommands = do
    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang == SystemDE) elNotImplemented

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
    elAttr "table"
           (  "class" =: "ploverCommands"
           <> "id"    =: "punctuation"
           ) $ do
        el "tr" $ do
            el "th" $ text "Short"
            el "th" $ text "Key"
            el "th" $ text "Description"
            el "th" $ text "Plover code"
        el "tr" $ do
            el "td" $ text ","
            el "td" $ el "code" $ text "A"
            el "td" $ text "attach comma"
            elClass "td" "plover" $ el "code" $ text "{^,}"
        el "tr" $ do
            el "td" $ text ";"
            el "td" $ el "code" $ text "NA"
            el "td" $ text "attach semicolon"
            elClass "td" "plover" $ el "code" $ text"{^;}"
        el "tr" $ do
            el "td" $ text "-"
            el "td" $ el "code" $ text "~"
            el "td" $ text "hyphen to attach words"
            elClass "td" "plover" $ el "code" $ text"{^-^}"
        el "tr" $ do
            el "td" $ text "\\t"
            el "td" $ el "code" $ text "DJ"
            el "td" $ text "tab like t"
            elClass "td" "plover" $ el "code" $ text"{^\\t^}"
        el "tr" $ do
            el "td" $ text " "
            el "td" $ el "code" $ text "B-"
            el "td" $ text "capitalize last word"
            elClass "td" "plover" $ el "code" $ text"{*-|}"
        el "tr" $ do
            el "td" $ text " "
            el "td" $ el "code" $ text "D-"
            el "td" $ text "capitalize next word"
            elClass "td" "plover" $ el "code" $ text"{-|}"
        el "tr" $ do
            el "td" $ text " "
            el "td" $ el "code" $ text "S-"
            el "td" $ text "uncapitalize last word"
            elClass "td" "plover" $ el "code" $ text"{*>}"
        el "tr" $ do
            el "td" $ text "␣"
            el "td" $ el "code" $ text "G-"
            el "td" $ text "retroactively add space"
            elClass "td" "plover" $ el "code" $ text"{*?}"
        el "tr" $ do
            el "td" $ text "\n"
            el "td" $ el "code" $ text "J"
            el "td" $ text "paragraph: period, two newlines"
            elClass "td" "plover" $ el "code" $ text"{^.\n\n^}{-|}"
        el "tr" $ do
            el "td" $ text " "
            el "td" $ el "code" $ text "F-"
            el "td" $ text "retroactively delete space"
            elClass "td" "plover" $ el "code" $ text"{*!}"
        el "tr" $ do
            el "td" $ text "."
            el "td" $ el "code" $ text "N-"
            el "td" $ text "full stop: period"
            elClass "td" "plover" $ el "code" $ text"{^.}{-|}"
        el "tr" $ do
            el "td" $ text ":"
            el "td" $ el "code" $ text "L-"
            el "td" $ text "attach colon"
            elClass "td" "plover" $ el "code" $ text"{^:}"
        el "tr" $ do
            el "td" $ text ":"
            el "td" $ el "code" $ text "JL-"
            el "td" $ text "colon and capitalize"
            elClass "td" "plover" $ el "code" $ text"{^:}{-|}"
        el "tr" $ do
            el "td" $ text "?"
            el "td" $ el "code" $ text "JN-"
            el "td" $ text "question mark and capitalize"
            elClass "td" "plover" $ el "code" $ text"{^?}{-|}"
        el "tr" $ do
            el "td" $ text "!"
            el "td" $ el "code" $ text "JR"
            el "td" $ text "exclamation mark and capitalize"
            elClass "td" "plover" $ el "code" $ text"{^!}{-|}"
        el "tr" $ do
            el "td" $ text "#"
            el "td" $ el "code" $ text "H"
            el "td" $ text "hash with next word attached"
            elClass "td" "plover" $ el "code" $ text"{\\#^}"
        el "tr" $ do
            el "td" $ text "§"
            el "td" $ el "code" $ text "BD-"
            el "td" $ text "legal paragraph symbol"
            elClass "td" "plover" $ el "code" $ text "§"
        el "tr" $ do
            el "td" $ text "°"
            el "td" $ el "code" $ text "GD-"
            el "td" $ text "attach degree symbol"
            elClass "td" "plover" $ el "code" $ text"{^°}"
        el "tr" $ do
            el "td" $ text "™"
            el "td" $ el "code" $ text "DM-"
            el "td" $ text "attach trademark symbol"
            elClass "td" "plover" $ el "code" $ text"{^™}"
        el "tr" $ do
            el "td" $ text "©"
            el "td" $ el "code" $ text "GDM-"
            el "td" $ text "attach copyright symbol"
            elClass "td" "plover" $ el "code" $ text"{^©}"
        el "tr" $ do
            el "td" $ text "€"
            el "td" $ el "code" $ text "E"
            el "td" $ text "euro symbol"
            elClass "td" "plover" $ el "code" $ text "€"
        el "tr" $ do
            el "td" $ text "—"
            el "td" $ el "code" $ text "~Ü"
            el "td" $ text "em dash"
            elClass "td" "plover" $ el "code" $ text "—"
        el "tr" $ do
            el "td" $ text "s"
            el "td" $ el "code" $ text "s"
            el "td" $ text "attach s and attach the next word"
            elClass "td" "plover" $ el "code" $ text "{^s^}"

    -- parentheses
    el "h3" $ text "Opening and closing"

    elClass "div" "paragraph" $
        text "The commands that follow adhere to a strict logic \
             \and thus aren't that scary. Your fingers of your right \
             \hand take care of any thing that opens and closes."

    elAttr "table"
           (  "class" =: "ploverCommands"
           <> "id"    =: "openingClosing"
           ) $ do
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
            elClass "td" "plover" $ do
                el "code" $ text "{«^}"
                text ","
                el "code" $ text "{^»}"
        el "tr" $ do
            el "td" $ text "„“"
            el "td" $ text "german quotes"
            el "td" $ el "code" $ text "-L"
            el "td" $ el "code" $ text "-N"
            elClass "td" "plover" $ do
                el "code" $ text "{„^}"
                text ","
                el "code" $ text "{^“}"
        el "tr" $ do
            el "td" $ text "‹›"
            el "td" $ text "chevrons"
            el "td" $ el "code" $ text "-M"
            el "td" $ el "code" $ text "-B"
            elClass "td" "plover" $ do
                el "code" $ text "{‹^}"
                text ","
                el "code" $ text "{^›}"
        el "tr" $ do
            el "td" $ text "[]"
            el "td" $ text "square brackets"
            el "td" $ el "code" $ text "-F"
            el "td" $ el "code" $ text "s"
            elClass "td" "plover" $ do
                el "code" $ text "{[^}"
                text ","
                el "code" $ text "{^]}"
        el "tr" $ do
            el "td" $ text "()"
            el "td" $ text "parenthesis"
            el "td" $ el "code" $ text "-S"
            el "td" $ el "code" $ text "-D"
            elClass "td" "plover" $ do
                el "code" $ text "{(^}"
                text ","
                el "code" $ text "{^)}"
        el "tr" $ do
            el "td" $ text "{}"
            el "td" $ text "brackets"
            el "td" $ el "code" $ text "ʃ"
            el "td" $ el "code" $ text "n"
            elClass "td" "plover" $ do
                el "code" $ text "{\\{^}"
                text ","
                el "code" $ text "{^\\}}"

    -- ascii smileys
    el "h3" $ text "ASCII art"

    elClass "div" "paragraph" $
        text "Steno typing lends itself well to all kinds of makros, \
             \among them I included my personal selection of ASCII \
             \emoticons."

    elAttr "table"
           (  "class" =: "ploverCommands"
           <> "id"    =: "asciiArt"
           ) $ do
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

    -- plover
    el "h3" $ text "Plover application shortcuts"

    elClass "div" "paragraph" $ do
        text "When actually using the Plover software instead of practicing \
             \on this website, you will find those shortcuts useful. They \
             \all follow the same scheme: "
        el "code" $ text "BDJN"
        text " plus some letter related to the plover command."

    elClass "table" "ploverCommands" $ do
        el "tr" $ do
            el "td" $ el "code" $ text "BDJN+D"
            elClass "td" "plover" $ el "code" $ text "{PLOVER:TOGGLE}"
        el "tr" $ do
            el "td" $ el "code" $ text "BDJNA"
            elClass "td" "plover" $ el "code" $ text "{PLOVER:ADD_TRANSLATION}"
        el "tr" $ do
            el "td" $ el "code" $ text "BDJNL"
            elClass "td" "plover" $ el "code" $ text "{PLOVER:LOOKUP}"
        el "tr" $ do
            el "td" $ el "code" $ text "BDJNS"
            elClass "td" "plover" $ el "code" $ text "{PLOVER:SUGGESTIONS}"
        el "tr" $ do
            el "td" $ el "code" $ text "BDJNF"
            elClass "td" "plover" $ el "code" $ text "{PLOVER:FOCUS}"
        el "tr" $ do
            el "td" $ el "code" $ text "BDJNG"
            elClass "td" "plover" $ el "code" $ text "{PLOVER:CONFIGURE}"
        el "tr" $ do
            el "td" $ el "code" $ text "BDJN+G"
            elClass "td" "plover" $ el "code" $ text "{PLOVER:CONFIGURE}"

    elClass "div" "paragraph" $ do
        text "Sooner or later you will add your own dictionary entries, or \
             \make adjustments to the default. The "
        elAttr "a" ("href" =: "https://github.com/openstenoproject/plover/wiki/Dictionary-Format"
                   ) $ text "Plover syntax"
        text " is documented well for exactly that purpose."

    el "h2" $ text "Practicing for the real-life"

    elClass "div" "paragraph" $ do
        text "There are more interactive exercises on this page, however, if \
             \you have not already done so, it's high time you actually install \
             \the "
        elAttr "a" ("href" =: "https://github.com/openstenoproject/plover/releases/tag/v4.0.0.dev10"
                   ) $ text "Plover Software"
        text "."

    elClass "div" "paragraph" $
        text "Plover Version 4 comes with a 'Plugin Manager' where you can find \
             \\"Palantype DE\". Try it out and type a few paragraphs!"

    pure envNavigation
