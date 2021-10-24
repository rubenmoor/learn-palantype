{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Clay          (keyframes, Normal(normal), ease, infinite, animation, lineHeight, li, ul, listStyleType, paddingTop, h3, darkblue, h2, overflowClip, bsInset, yellow, borderSpacing, paddingRight, paddingLeft, html, Initial(initial), footer, overflowY, All (all), Auto (auto), Center (center), Color,
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
                                lightgreen, linear, margin,
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
import           Clay.Flexbox  (row, nowrap)
import qualified Clay.Media    as Media
import           Data.Function (($))
import           GHC.IO        (IO)
import           GHC.Num       (Num ((*), (+), (-)))
import           System.IO     (putStrLn)
import Control.Applicative (Applicative(pure))
import Data.Monoid (Monoid(mempty))

anthrazit :: Color
anthrazit = rgb 8 20 48 -- #081430;

myLightgray :: Color
myLightgray = rgb 230 230 230

colorLink :: Color
colorLink = blue

colorLinkVisited :: Color
colorLinkVisited = rgb 64 103 124

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
      visited & color colorLinkVisited
      ".normalLink" & do
        color colorLink
        cursor pointer
    height $ pct 100
    h2 ? color darkblue
    h3 ? color anthrazit
  star ? boxSizing borderBox

  -- flex layout

  div # ".box" ? do
    display flex
    flexFlow column nowrap
    height $ pct 100

  header ? do
    flexGrow 0
    flexShrink 1
    -- already the default? flexBasis auto

  div # ".row" ? do
    display flex
    flexFlow row nowrap
    flexGrow 1
    flexShrink 1
    overflowY hidden

  section # ".content" ? do
    overflowY auto
    paddingLeft $ em 1
    paddingRight $ em 1
    height $ pct 100

  section # ".toc" ? do
    marginTop $ em 0.83
    paddingLeft $ px 12
    paddingTop $ px 12
    flexShrink 0
    div ? marginRight (px 12)
    ul ? do
      listStyleType none
      margin (px 0) (px 0) (px 0) (px 0)
      padding (px 0) (px 0) (px 0) (px 0)
      li ? do
        lineHeight $ em 1.8
        ".stage" & do
          cursor pointer
          marginTop $ em 0.2
          i ? do
            color anthrazit
            fontSize $ pt 18
        span ? do
          paddingLeft $ px 8
          paddingRight $ px 8
          color gray
        i ? do
          color green
          paddingLeft $ px 8
          paddingRight $ px 8
          fontSize $ pt 14

  footer ? do
    flexGrow 0
    flexShrink 1
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
    color myLightgray
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
      thumbrowOffset = 12

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

  div # ".keyboard" ? do
    width $ px keyboardWidth
    height $ px keyboardHeight
    position relative

    span # ".system" ? do
      position absolute
      top $ px 12
      left $ px 0
      fontSize $ pt 12
      color anthrazit
      textAlign center
      width $ px keyboardWidth

    table ? do
      borderSpacing $ px 4
      width $ px keyboardWidth
      height $ px keyboardHeight
      borderRadius (px 8) (px 8) (px 8) (px 8)
      backgroundColor myLightgray
      padding (px keyboardPadding)
              (px keyboardPadding)
              (px $ keyboardPadding + thumbrowOffset)
              (px keyboardPadding)

      td ? do
        borderRadius (px 4) (px 4) (px 4) (px 4)
        border solid (px 1) gray
        backgroundColor white
        width $ pct 8.11
        height $ pct 25
        textAlign center
        let shadow = bsColor anthrazit $ shadowWithSpread (px 4) (px 4) (px 8) (px 0)
        boxShadow $ pure shadow

        ".gap" & visibility hidden
        ".handgap" & do
          visibility hidden
          width $ pct 2.70

        ".thumbrow" & do
          position relative
          top (px 12)

        ".pressed" & do
          color blue
          boxShadow $ pure none
        let shadowInset = bsInset $ bsColor (rgb 220 220 255) $ shadowWithSpread (px 0) (px 0) (px 8) (px 6)
        ".homerow" & boxShadow [shadow, shadowInset]
        ".pressed.homerow" & boxShadow (pure shadowInset)
        ".inactive" & do
          backgroundColor myLightgray
          color anthrazit
          div # ".steno" ? visibility hidden
        ".inactive.pressed" & boxShadow (pure none)

        div ? do
          ".steno" & do
            fontWeight bold
            fontSize $ pt 18
          ".qwerty" & do
            fontSize $ pt 10
            color gray
            marginTop $ px (-6)

  span # ".btnHeader" ? do
    color gray
    fontSize $ pt 24
    padding (px 8) (px 8) (px 8) (px 8)
    ".keyboardVisible" & do
      backgroundColor myLightgray
      color white

  span # ".btn" ? do
    color gray
    fontSize $ pt 24

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
  ".bgLightgray" ? backgroundColor myLightgray
  ".fgTransparent" ? color transparent
  ".red" ? color red
  ".small" ? fontSize (pt 12)
  ".anthrazit" ? color anthrazit

  pre ? do
    backgroundColor myLightgray
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

  ".blinking" ? animation "blink" (sec 1) ease (sec 0) infinite normal none

  keyframes "blink" [(0, opacity 0), (0.5, opacity 1), (1, opacity 0)]
