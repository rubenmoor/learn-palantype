{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Clay          (Auto (auto), Center (center), Color, Css,
                                Cursor (cursor), None (none), a, absolute,
                                after, alignItems, backgroundColor, block, body,
                                bold, border, borderBox, borderRadius, both,
                                bottom, boxShadow, boxSizing, bsColor, button,
                                clear, color, content, darkgray, display,
                                displayTable, div, easeInOut, em, fixed, flex,
                                float, floatLeft, floatRight, focus, fontFamily,
                                fontSize, fontSizeCustom, fontWeight, gray,
                                height, hidden, hover, i, important,
                                inlineBlock, inlineFlex, input, justifyContent,
                                left, lightblue, lightgray, lightgrey, margin,
                                marginBottom, marginRight, marginTop, maxWidth,
                                minHeight, minWidth, opacity, outline, overflow,
                                padding, paddingBottom, pct, pointer, position,
                                pre, preWrap, pt, putCss, px, queryOnly, red,
                                relative, rgb, rgba, right, sansSerif, sec,
                                shadowWithBlur, shadowWithSpread, smaller,
                                solid, span, star, stringContent, table, td,
                                textAlign, top, transform, transition,
                                translate, transparent, visibility, visited,
                                white, whiteSpace, width, zIndex, (#), (&), (?),
                                (^=), (|>))
import qualified Clay.Media    as Media
import           Data.Function (($))
import           GHC.IO        (IO)
import           GHC.Num       (Num ((*), (+), (-)))
import           System.IO     (putStrLn)
import Clay.Border (borderBottom)

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

  let keyboardWidth = 650
      keyboardHeight = 350
      keyboardPadding = 12
      stenoOutputHeight = 71

  td # ".gap" ? visibility hidden
  div # ".keyboard" ? do
    width $ px keyboardWidth
    height $ px keyboardHeight
    position relative
    borderBottom solid (px 1) darkgray

    input ? position absolute
    table ? position absolute

    span ? do
      position absolute
      width $ px keyboardWidth
      left $ px 0

      ".stenoOutput" & do
        textAlign center
        top $ px $ keyboardHeight - stenoOutputHeight
        fontSize $ pt 32
        padding (px 8) (px 8) (px 8) (px 8)
      ".clickMe" & do
        top $ px 12
        fontSize $ pt 24
        textAlign center
      ".clickMe.red" & color red

      ".system" & do
        top $ px $ keyboardHeight - 32 - stenoOutputHeight
        left $ px 12
        fontSize $ pt 12
        color anthrazit

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
      width $ px keyboardWidth
      height $ px $ keyboardHeight - stenoOutputHeight
      borderRadius (px 8) (px 8) (px 8) (px 8)
      backgroundColor lightgray
      padding (px keyboardPadding)
              (px keyboardPadding)
              (px keyboardPadding)
              (px keyboardPadding)

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

  span # ".btnToggleKeyboard" ? do
    color gray
    fontSize $ pt 24
    padding (px 8) (px 8) (px 8) (px 8)
    ".keyboardVisible" & do
      backgroundColor lightgray
      color white

  div # ".dropdown" ? do
    position relative
    display inlineBlock
    span # ".dropdown-button" ? do
      color gray
      fontSize $ pt 24
      padding (px 8) (px 8) (px 8) (px 8)
    div # ".dropdown-content" ? do
      display none
      position absolute
      backgroundColor $ rgb 249 249 249
      minWidth $ px 160
      boxShadow [bsColor (rgba 0 0 0 0.2) $ shadowWithSpread (px 0) (px 8) (px 16) (px 0)]
      zIndex 1
      span ? do
        minWidth $ px 160
        padding (px 12) (px 12) (px 12) (px 12)
        display block
        hover & backgroundColor gray

    ":hover" & div # ".dropdown-content" ? display block

  -- file input with custom look
  -- cf. https://stackoverflow.com/a/29251314/2925659
  span # ".hiddenFileInput" ? do
    position relative
    input ? do -- height (pct 100)
      position absolute
      top $ px 0
      left $ px 0
      height $ pct 100
      opacity 0
    display inlineBlock
    overflow hidden
