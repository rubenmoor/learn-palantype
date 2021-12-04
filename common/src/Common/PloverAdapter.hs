{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Common.PloverAdapter where

import           Data.Either                    ( Either(..) )
import           Data.Foldable                  ( Foldable(foldl') )
import           Data.Function                  ( ($) )
import qualified Data.IntMap.Strict            as IntMap
import           Data.List                      ( (++) )
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import           GHC.Err                        ( error )
import           Web.KeyCode                    ( Key(..)
                                                , KeyCode
                                                , keyCodeMap
                                                )

toKeyCodes :: Key -> [KeyCode]
toKeyCodes k = fromMaybe (error "toKeyCodes: impossible") $ Map.lookup k m
  where
    m :: Map Key [KeyCode]
    m = foldl' accInsert Map.empty $ IntMap.toList keyCodeMap

    accInsert m' (c, key) = Map.insertWith (++) key [c] m'

fromPlover :: Text -> Either Text [KeyCode]
fromPlover = \case
    "BackSpace" -> Right $ toKeyCodes Backspace
    "Tab"       -> Right $ toKeyCodes Tab
    "Return"    -> Right $ toKeyCodes Enter
    "Escape"    -> Right $ toKeyCodes Escape
    "space"     -> Right $ toKeyCodes Space
    "Page_Up"   -> Right $ toKeyCodes PageUp
    "Page_Down" -> Right $ toKeyCodes PageDown
    "End"       -> Right $ toKeyCodes End
    "Home"      -> Right $ toKeyCodes Home
    "Left"      -> Right $ toKeyCodes ArrowLeft
    "Up"        -> Right $ toKeyCodes ArrowUp
    "Right"     -> Right $ toKeyCodes ArrowRight
    "Down"      -> Right $ toKeyCodes ArrowDown
    "Insert"    -> Right $ toKeyCodes Insert
    "Delete"    -> Right $ toKeyCodes Delete
    "0"         -> Right $ toKeyCodes Digit0
    "1"         -> Right $ toKeyCodes Digit1
    "2"         -> Right $ toKeyCodes Digit2
    "3"         -> Right $ toKeyCodes Digit3
    "4"         -> Right $ toKeyCodes Digit4
    "5"         -> Right $ toKeyCodes Digit5
    "6"         -> Right $ toKeyCodes Digit6
    "7"         -> Right $ toKeyCodes Digit7
    "8"         -> Right $ toKeyCodes Digit8
    "9"         -> Right $ toKeyCodes Digit9
    "a"         -> Right $ toKeyCodes KeyA
    "b"         -> Right $ toKeyCodes KeyB
    "c"         -> Right $ toKeyCodes KeyC
    "d"         -> Right $ toKeyCodes KeyD
    "e"         -> Right $ toKeyCodes KeyE
    "f"         -> Right $ toKeyCodes KeyF
    "g"         -> Right $ toKeyCodes KeyG
    "h"         -> Right $ toKeyCodes KeyH
    "i"         -> Right $ toKeyCodes KeyI
    "j"         -> Right $ toKeyCodes KeyJ
    "k"         -> Right $ toKeyCodes KeyK
    "l"         -> Right $ toKeyCodes KeyL
    "m"         -> Right $ toKeyCodes KeyM
    "n"         -> Right $ toKeyCodes KeyN
    "o"         -> Right $ toKeyCodes KeyO
    "p"         -> Right $ toKeyCodes KeyP
    "q"         -> Right $ toKeyCodes KeyQ
    "r"         -> Right $ toKeyCodes KeyR
    "s"         -> Right $ toKeyCodes KeyS
    "t"         -> Right $ toKeyCodes KeyT
    "u"         -> Right $ toKeyCodes KeyU
    "v"         -> Right $ toKeyCodes KeyV
    "w"         -> Right $ toKeyCodes KeyW
    "x"         -> Right $ toKeyCodes KeyX
    "y"         -> Right $ toKeyCodes KeyY
    "z"         -> Right $ toKeyCodes KeyZ
    "F1"        -> Right $ toKeyCodes F1
    "F2"        -> Right $ toKeyCodes F2
    "F3"        -> Right $ toKeyCodes F3
    "F4"        -> Right $ toKeyCodes F4
    "F5"        -> Right $ toKeyCodes F5
    "F6"        -> Right $ toKeyCodes F6
    "F7"        -> Right $ toKeyCodes F7
    "F8"        -> Right $ toKeyCodes F8
    "F9"        -> Right $ toKeyCodes F9
    "F10"       -> Right $ toKeyCodes F10
    "F11"       -> Right $ toKeyCodes F11
    "F12"       -> Right $ toKeyCodes F12
    ";"         -> Right $ toKeyCodes Semicolon
    "="         -> Right $ toKeyCodes Equals
    ","         -> Right $ toKeyCodes Comma
    "-"         -> Right $ toKeyCodes Subtract
    "."         -> Right $ toKeyCodes Period
    "/"         -> Right $ toKeyCodes ForwardSlash
    "`"         -> Right $ toKeyCodes Backquote
    "["         -> Right $ toKeyCodes BracketLeft
    "\\"        -> Right $ toKeyCodes Backslash
    "]"         -> Right $ toKeyCodes BracketRight
    "'"         -> Right $ toKeyCodes Apostrophe
    -- [chrome, firefox]
    "ü"         -> Right [186, 219]
    "ö"         -> Right [192, 59]
    "ä"         -> Right [222]
    "ß"         -> Right [219, 63]
    str         -> Left str
