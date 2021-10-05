{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Clay          (paddingRight, paddingLeft, html, Initial(initial), footer, overflowY, All (all), Auto (auto), Center (center), Color,
                                Css, Cursor (cursor), None (none),
                                Other (other), a, absolute, after, alignItems,
                                backgroundColor, block, blue, body, bold,
                                border, borderBox, borderRadius, both, bottom,
                                boxShadow, boxSizing, bsColor, button, clear,
                                code, color, column, content, darkgray, display,
                                displayTable, div, easeInOut, em, fixed, flex,
                                flexBasis, flexFlow, flexGrow, flexShrink,
                                float, floatLeft, floatRight, focus, fontFamily,
                                fontSize, fontSizeCustom, fontWeight, gray,
                                green, header, height, hidden, hover, i,
                                important, inlineBlock, inlineFlex, input,
                                justifyContent, left, lightblue, lightgray,
                                lightgreen, lightgrey, linear, margin,
                                marginBottom, marginRight, marginTop, maxWidth,
                                minHeight, minWidth, opacity, outline, overflow,
                                padding, paddingBottom, pct, pointer, position,
                                pre, preWrap, pt, putCss, px, queryOnly, red,
                                relative, rgb, rgba, right, sansSerif, sec,
                                section, shadowWithBlur, shadowWithSpread,
                                small, smaller, solid, span, star,
                                stringContent, table, td, textAlign,
                                textDecoration, top, transform, transition,
                                translate, transparent, underline, visibility,
                                visited, white, whiteSpace, width, zIndex,
                                ( # ), (&), (?), (^=), (|>))
import           Clay.Border   (borderBottom)
import           Clay.Flexbox  (nowrap)
import qualified Clay.Media    as Media
import           Data.Function (($))
import           GHC.IO        (IO)
import           GHC.Num       (Num ((*), (+), (-)))
import           System.IO     (putStrLn)

anthrazit :: Color
anthrazit = rgb 8 20 48 -- #081430;

colorLink :: Color
colorLink = blue

main :: IO ()
main = putCss $ do
  html ? height (pct 100)
  body ? do
    fontFamily ["Abel"] [sansSerif]
    fontSize $ pt 18
    margin (px 0) (px 0) (px 0) (px 0)
    a ? do
      ":link" & do
        textDecoration none
        color colorLink
      ":hover" & textDecoration underline
      ".normalLink" & do
        color colorLink
        cursor pointer
    height $ pct 100
  star ? boxSizing borderBox

  -- flex layout

  div # ".box" ? do
    display flex
    flexFlow column nowrap
    height $ pct 100

  header ? do
    flexGrow 0
    flexShrink 1
    flexBasis auto

  section ? do
    display flex
    flexFlow column nowrap
    flexGrow 1
    flexShrink 1
    flexBasis auto
    overflowY auto
    paddingLeft $ em 1
    paddingRight $ em 1

  footer ? do
    flexGrow 0
    flexShrink 1
    flexBasis auto
    boxShadow [bsColor (rgba 0 0 0 0.2) $ shadowWithSpread (px 0) (px 8) (px 16) (px 0)]
    fontSize $ pt 14
    padding (px 12) (px 12) (px 12) (px 12)
    textAlign center

  div # ".mkOverlay" ? do
    top $ pct 50
    left $ pct 50
    transform (translate (pct $ -50) $ pct $ -50)
    maxWidth $ px 418
    backgroundColor white
    boxShadow [bsColor (rgba 0 0 0 0.2) $ shadowWithSpread (px 0) (px 8) (px 16) (px 0)]
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
      keyboardHeight = 271
      keyboardPadding = 12

  div # ".stenoInput" ? do
    width $ px keyboardWidth
    -- input ? position absolute
    borderBottom solid (px 1) darkgray
    input ? do
      width $ px keyboardWidth
      textAlign center
      fontSize $ pt 32
      padding (px 8) (px 8) (px 8) (px 8)
      border none (px 0) transparent
      --backgroundColor transparent
      -- input # focus ? outline none (px 0) transparent
      ":focus-visible" & outline none (px 0) transparent

  td # ".gap" ? visibility hidden
  div # ".keyboard" ? do
    width $ px keyboardWidth
    height $ px keyboardHeight
    position relative

    span # ".system" ? do
      position absolute
      top $ px $ keyboardHeight - 32
      left $ px 12
      fontSize $ pt 12
      color anthrazit

    table ? do
      width $ px keyboardWidth
      height $ px keyboardHeight
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
      fontSize (pt 12)
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

  ".bgWhite" ? backgroundColor white
  ".bgGreen" ? backgroundColor lightgreen
  ".bgRed" ? backgroundColor red
  ".fgTransparent" ? color transparent
  ".red" ? color red
  ".small" ? fontSize (pt 12)
  ".anthrazit" ? color anthrazit

  pre ? do
    backgroundColor lightgray
    width $ other "fit-content"
    borderRadius (px 4) (px 4) (px 4) (px 4)
    padding (px 6) (px 6) (px 6) (px 6)
    span ? padding (px 2) (px 2) (px 2) (px 2)

  div # ".congraz" ? do
    textAlign center
    i ? do
      fontSize $ pt 72
      color green
      padding (px 8) (px 8) (px 8) (px 8)

  div # ".paragraph" ? marginBottom (em 0.5)

  ".floatLeft" ? float floatLeft
  ".floatRight" ? float floatRight
  ".clearBoth" ? clear both

  div # ".start" ? do
    textAlign center
    margin (px 24) (px 24) (px 24) (px 24)

  button # ".start" ? do
    backgroundColor anthrazit
    color white
    fontSize $ pt 24
    fontWeight bold
    borderRadius (px 12) (px 12) (px 12) (px 12)
    padding (px 12) (px 12) (px 12) (px 12)
    border none (px 0) transparent
    cursor pointer

  ".displayNone" ? display none
