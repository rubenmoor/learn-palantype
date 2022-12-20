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

module Page.Stage40.SpecialCharacters
    ( specialCharacters
    ) where

import Control.Applicative (Applicative (pure))
import Control.Monad (unless)
import Control.Monad.Reader.Class (MonadReader, ask)
import Data.Function (($))
import Data.Semigroup ((<>))
import Page.Common (elNotImplemented)
import Palantype.Common (Lang (DE))
import Reflex.Dom ((=:), DomBuilder, blank, el, elAttr, elClass, text)
import State (Env (..), Navigation (..))
import Obelisk.Generated.Static (static)
import Data.Eq (Eq((==)))

specialCharacters
  :: forall key t (m :: * -> *)
  . ( DomBuilder t m
    , MonadReader (Env t key) m
    )
  => m Navigation
specialCharacters = do
    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang == DE) elNotImplemented

    el "h1" $ text "Typing numbers"

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

    elClass "blockquote" "warning" $ do
        el "strong" $ text "Not implemented"
        el "br" blank
        text "There hasn't been any exercise implemented yet"

    pure envNavigation
