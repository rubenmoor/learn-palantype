{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Clay          (preWrap, pre, whiteSpace, i, overflow, inlineBlock, (&), (|>), opacity, red, transparent, Auto (auto), Center (center), Color, Css,
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

anthrazit :: Color
anthrazit = rgb 8 20 48 -- #081430;

main :: IO ()
main = putCss $ do
  body ? do
    fontFamily ["Abel"] [sansSerif]
    margin (px 0) (px 0) (px 0) (px 0)
  star ? boxSizing borderBox

  div # ".mkOverlay" ? do
    top $ pct 50
    left $ pct 50
    transform (translate (pct $ -50) $ pct $ -50)
    maxWidth $ px 418
    backgroundColor white
    padding (px 24) (px 24) (px 24) (px 24)
    position absolute
    zIndex 1

  div # ".msgOverlay" ? do
    bottom $ px 0
    left $ pct 50
    transform (translate (pct $ -50) $ pct 0)
    maxWidth $ px 418
    borderRadius (px 12) (px 12) (px 12) (px 12)
    backgroundColor anthrazit
    color lightgrey
    fontSize (pt 12)
    padding (px 12) (px 12) (px 12) (px 12)
    position absolute
    zIndex 0
    span ? whiteSpace preWrap
    div ? fontWeight bold

  span # ".close" ? do
    float floatRight
    cursor pointer

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
    position relative

    input ? position absolute
    table ? position absolute

    span ? do
      position absolute
      width $ px keyboardWidth
      textAlign center
      left $ px 0
      top $ px 12
      fontSize $ pt 24

      ".red" & color red

      ".steno" & do
        top $ px keyboardHeight
        fontSize $ pt 32
        padding (px 8) (px 8) (px 8) (px 8)

    input ? do
      width $ px keyboardWidth
      height $ px keyboardHeight
      top $ px 0
      left $ px 0
      backgroundColor transparent
      border none (px 0) transparent

      -- input # focus ? outline none (px 0) transparent

      ":focus-visible" & outline none (px 0) transparent

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

        ".pressed" & backgroundColor lightblue

        div ? do
          ".steno" & do
            fontWeight bold
            fontSize $ pt 18
          ".qwerty" & do
            fontSize $ pt 10
            color gray
            marginTop $ px (-6)


  -- file input with custom look
  -- cf. https://stackoverflow.com/a/29251314/2925659
  span # ".hiddenFileInput" ? do
    let size = 60
    position relative
    star ? do
      position absolute
      top (px 0)
      left (px 0)
    input ? do
      height (pct 100)
      width (px size)
      opacity 0
      cursor pointer
    width (px size)
    height (px size)
    color gray
    fontSize (px size)
    display inlineBlock
    overflow hidden
