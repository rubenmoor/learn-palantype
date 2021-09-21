{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Clay          (red, transparent, Auto (auto), Center (center), Color, Css,
                                Cursor (cursor), None (none), a, absolute,
                                after, alignItems, backgroundColor, block, body,
                                bold, border, borderBox, borderRadius, both,
                                bottom, boxShadow, boxSizing, bsColor, button,
                                clear, color, content, darkgray, display,
                                displayTable, div, easeInOut, em, fixed, flex,
                                float, floatLeft, floatRight, focus, fontFamily,
                                fontSize, fontSizeCustom, fontWeight, gray,
                                height, hidden, important, inlineFlex, input,
                                justifyContent, left, lightblue, lightgray,
                                lightgrey, margin, marginBottom, marginRight,
                                marginTop, maxWidth, minHeight, outline,
                                padding, paddingBottom, pct, pointer, position,
                                pt, putCss, px, queryOnly, relative, rgb, right,
                                sansSerif, sec, shadowWithBlur, smaller, solid,
                                span, star, stringContent, table, td, textAlign,
                                top, transform, transition, translate,
                                visibility, visited, white, width, zIndex,
                                ( # ), (?), (^=))
import qualified Clay.Media    as Media
import           Data.Function (($))
import           GHC.IO        (IO)
import           System.IO     (putStrLn)
import GHC.Num (Num((-), (*), (+)))

main :: IO ()
main = putCss $ do
  body ? do
    fontFamily ["Abel"] [sansSerif]
    margin (px 0) (px 0) (px 0) (px 0)
  star ? boxSizing borderBox

  let keyboardWidth = 638
      keyboardHeight = 242
      keyboardPadding = 12

  td # ".gap" ? visibility hidden
  div # ".keyboard" ? do
    borderRadius (px 8) (px 8) (px 8) (px 8)
    backgroundColor lightgray
    width $ px keyboardWidth
    height $ px keyboardHeight
    padding (px keyboardPadding)
            (px keyboardPadding)
            (px keyboardPadding)
            (px keyboardPadding)

    span ? do
      position absolute
      width $ px keyboardWidth
      textAlign center
      top $ px 12
      fontSize $ pt 24
    span # ".red" ? color red

    input ? do
      width $ px keyboardWidth
      height $ px keyboardHeight
      top $ px 0
      left $ px 0
      backgroundColor transparent
      border none (px 0) transparent
    -- input # focus ? outline none (px 0) transparent
    input # ":focus-visible" ? outline none (px 0) transparent

    table ? do
      width $ px $ keyboardWidth - 2.0 * keyboardPadding
      height $ px $ keyboardHeight - 2.0 * keyboardPadding
      td ? do
        borderRadius (px 4) (px 4) (px 4) (px 4)
        border solid (px 1) gray
        backgroundColor white
        width $ pct 8.33
        height $ pct 25
        textAlign center
        div # ".steno" ? do
          fontWeight bold
          fontSize $ pt 18
        div # ".qwerty" ? do
          fontSize $ pt 10
          color gray
          marginTop $ px (-6)
      td # ".pressed" ? backgroundColor lightblue
    position relative
    input ? position absolute
    table ? position absolute
