{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Common.Keys where

import           Data.Maybe  (Maybe (..))
import           Web.KeyCode (Key (..))
import Data.String (String)

fromPlover :: String -> Maybe Key
fromPlover = \case
  "BackSpace" -> Just Backspace
  "Tab"       -> Just Tab
  "Return"    -> Just Enter
  "Escape"    -> Just Escape
  "space"     -> Just Space
  "Page_Up"   -> Just PageUp
  "Page_Down" -> Just PageDown
  "End"       -> Just End
  "Home"      -> Just Home
  "Left"      -> Just ArrowLeft
  "Up"        -> Just ArrowUp
  "Right"     -> Just ArrowRight
  "Down"      -> Just ArrowDown
  "Insert"    -> Just Insert
  "Delete"    -> Just Delete
  "0"         -> Just Digit0
  "1"         -> Just Digit1
  "2"         -> Just Digit2
  "3"         -> Just Digit3
  "4"         -> Just Digit4
  "5"         -> Just Digit5
  "6"         -> Just Digit6
  "7"         -> Just Digit7
  "8"         -> Just Digit8
  "9"         -> Just Digit9
  "a"         -> Just KeyA
  "b"         -> Just KeyB
  "c"         -> Just KeyC
  "d"         -> Just KeyD
  "e"         -> Just KeyE
  "f"         -> Just KeyF
  "g"         -> Just KeyG
  "h"         -> Just KeyH
  "i"         -> Just KeyI
  "j"         -> Just KeyJ
  "k"         -> Just KeyK
  "l"         -> Just KeyL
  "m"         -> Just KeyM
  "n"         -> Just KeyN
  "o"         -> Just KeyO
  "p"         -> Just KeyP
  "q"         -> Just KeyQ
  "r"         -> Just KeyR
  "s"         -> Just KeyS
  "t"         -> Just KeyT
  "u"         -> Just KeyU
  "v"         -> Just KeyV
  "w"         -> Just KeyW
  "x"         -> Just KeyX
  "y"         -> Just KeyY
  "z"         -> Just KeyZ
  "F1"        -> Just F1
  "F2"        -> Just F2
  "F3"        -> Just F3
  "F4"        -> Just F4
  "F5"        -> Just F5
  "F6"        -> Just F6
  "F7"        -> Just F7
  "F8"        -> Just F8
  "F9"        -> Just F9
  "F10"       -> Just F10
  "F11"       -> Just F11
  "F12"       -> Just F12
  ";"         -> Just Semicolon
  "="         -> Just Equals
  ","         -> Just Comma
  "-"         -> Just Subtract
  "."         -> Just Period
  "/"         -> Just ForwardSlash
  "`"         -> Just Backquote
  "["         -> Just BracketLeft
  "\\"        -> Just Backslash
  "]"         -> Just BracketRight
  "'"         -> Just Apostrophe
  _           -> Nothing
