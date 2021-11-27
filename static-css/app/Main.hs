{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Clay                           ((-:), sticky, before, inline, blockquote,  (#)
                                                , (&)
                                                , (?)
                                                , All(all)
                                                , Auto(auto)
                                                , Center(center)
                                                , Color
                                                , Css
                                                , Cursor(cursor)
                                                , Initial(initial)
                                                , None(none)
                                                , Normal(normal)
                                                , Other(other)
                                                , (^=)
                                                , a
                                                , absolute
                                                , after
                                                , alignItems
                                                , animation
                                                , backgroundColor
                                                , black
                                                , block
                                                , blue
                                                , body
                                                , bold
                                                , border
                                                , borderBox
                                                , borderRadius
                                                , borderSpacing
                                                , both
                                                , bottom
                                                , boxShadow
                                                , boxSizing
                                                , bsColor
                                                , bsInset
                                                , button
                                                , clear
                                                , code
                                                , color
                                                , column
                                                , content
                                                , darkblue
                                                , display
                                                , displayTable
                                                , div
                                                , ease
                                                , easeInOut
                                                , em
                                                , fixed
                                                , flex
                                                , flexBasis
                                                , flexFlow
                                                , flexGrow
                                                , flexShrink
                                                , float
                                                , floatLeft
                                                , floatRight
                                                , focus
                                                , fontFamily
                                                , fontSize
                                                , fontSizeCustom
                                                , fontStyle
                                                , fontWeight
                                                , footer
                                                , gray
                                                , green
                                                , h2
                                                , h3
                                                , header
                                                , height
                                                , hidden
                                                , hover
                                                , html
                                                , i
                                                , img
                                                , important
                                                , infinite
                                                , inlineBlock
                                                , inlineFlex
                                                , input
                                                , italic
                                                , justifyContent
                                                , keyframes
                                                , left
                                                , li
                                                , lightblue
                                                , lightgray
                                                , lightgreen
                                                , lineHeight
                                                , linear
                                                , listStyleType
                                                , margin
                                                , marginBottom
                                                , marginLeft
                                                , marginRight
                                                , marginTop
                                                , maxWidth
                                                , minHeight
                                                , minWidth
                                                , opacity
                                                , outline
                                                , overflow
                                                , overflowClip
                                                , overflowY
                                                , padding
                                                , paddingBottom
                                                , paddingLeft
                                                , paddingRight
                                                , paddingTop
                                                , pct
                                                , pointer
                                                , position
                                                , pre
                                                , preWrap
                                                , pt
                                                , putCss
                                                , px
                                                , queryOnly
                                                , red
                                                , relative
                                                , rgb
                                                , rgba
                                                , right
                                                , sansSerif
                                                , sec
                                                , section
                                                , shadowWithBlur
                                                , shadowWithSpread
                                                , small
                                                , smaller
                                                , solid
                                                , span
                                                , star
                                                , stringContent
                                                , table
                                                , td
                                                , textAlign
                                                , textDecoration
                                                , top
                                                , transform
                                                , transition
                                                , translate
                                                , transparent
                                                , ul
                                                , underline
                                                , visibility
                                                , visited
                                                , white
                                                , whiteSpace
                                                , width
                                                , yellow
                                                , zIndex
                                                , (|>)
                                                )
import           Clay.Border                    (borderTop,  borderBottom
                                                , dotted
                                                )
import           Clay.Flexbox                   ( nowrap
                                                , row
                                                )
import qualified Clay.Media                    as Media
import           Control.Applicative            ( Applicative(pure) )
import           Data.Function                  ( ($) )
import           Data.Monoid                    ( Monoid(mempty) )
import           GHC.IO                         ( IO )
import           GHC.Num                        ( Num((*), (+), (-), negate) )
import           System.IO                      ( putStrLn )

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

    section # "#content" ? do
        overflowY auto
        "scroll-behavior" -: "smooth"
        paddingLeft $ em 1
        paddingRight $ em 1
        height $ pct 100
        position relative

    section # "#content" # before ? do
      content $ stringContent "Up \\25B2 DMKSD" -- U+25B2
      -- content $ stringContent "Up ▲ DMKSD" -- unicode makes `ob run` crash
      display block
      width $ other "fit-content"
      position sticky
      top $ px 2
      fontSize $ pt 12
      fontWeight bold
      height $ px 21
      marginLeft auto
      marginRight auto
      backgroundColor $ rgb 102 141 60
      opacity 0.7
      color white
      borderRadius (px 8) (px 8) (px 8) (px 8)
      padding (px 4) (px 4) (px 4) (px 4)

    section # "#content" |> div # before ? do
      content $ stringContent ""
      width $ pct 80
      backgroundColor white
      position absolute
      top $ px 2
      height $ px 29

    section # "#content" # after ? do
      content $ stringContent "Down \\25BC JMKSD"
      display block
      width $ other "fit-content"
      position sticky
      bottom $ px 8
      fontSize $ pt 12
      fontWeight bold
      height $ px 21
      marginLeft auto
      marginRight auto
      backgroundColor $ rgb 102 141 60
      opacity 0.7
      color white
      borderRadius (px 8) (px 8) (px 8) (px 8)
      padding (px 4) (px 4) (px 4) (px 4)

    section # "#content" |> div # after ? do
      content $ stringContent ""
      width $ pct 80
      backgroundColor white
      position absolute
      height $ px 37
      zIndex 1

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

    footer # ".stage" ? do
        flexGrow 0
        flexShrink 1
        boxShadow
            [ bsColor (rgba 0 0 0 0.2)
                  $ shadowWithSpread (px 0) (px 8) (px 16) (px 0)
            ]
        fontSize $ pt 14
        padding (px 12) (px 12) (px 12) (px 12)
        textAlign center
        zIndex 1

    div # ".mkOverlay" ? do
        top $ pct 50
        left $ pct 50
        transform (translate (pct $ -50) $ pct $ -50)
        maxWidth $ px 418
        backgroundColor white
        boxShadow
            [ bsColor (rgba 0 0 0 0.2)
                  $ shadowWithSpread (px 0) (px 8) (px 16) (px 0)
            ]
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

    let keyboardWidth   = 650
        keyboardHeight  = 271
        keyboardPadding = 12
        thumbrowOffset  = 12
        pinkyYOffset    = 16

    div # ".stenoInput" ? do
        maxWidth $ pct 80
        minWidth (px keyboardWidth)
        marginLeft auto
        marginRight auto
        textAlign center
        borderBottom dotted (px 1) lightgray
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
        marginLeft auto
        marginRight auto

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
                let
                    shadow = bsColor anthrazit
                        $ shadowWithSpread (px 3) (px 3) (px 5) (px 0)
                boxShadow $ pure shadow

                ".gap" & visibility hidden
                ".handgap" & do
                    visibility hidden
                    width $ pct 2.70

                ".thumbrow" & do
                    position relative
                    top (px thumbrowOffset)

                ".pinkyYOffset" & do
                    position relative
                    top (px pinkyYOffset)

                ".pressed" & do
                    color blue
                    boxShadow $ pure none
                let shadowInset =
                        bsInset $ bsColor (rgb 220 220 255) $ shadowWithSpread
                            (px 0)
                            (px 0)
                            (px 8)
                            (px 6)
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
            boxShadow
                [ bsColor (rgba 0 0 0 0.2)
                      $ shadowWithSpread (px 0) (px 8) (px 16) (px 0)
                ]
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

    blockquote # ".warning" ? do
      backgroundColor lightblue
      padding (px 36) (px 36) (px 36) (px 36)

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
    ".inline" ? display inline
    ".inlineBlock" ? display inlineBlock

    ".blinking" ? animation "blink" (sec 1) ease (sec 0) infinite normal none

    keyframes "blink" [(0, opacity 0), (0.5, opacity 1), (1, opacity 0)]

    div # ".landing" ? do
        textAlign center
        img # ".getstartedhere" ? do
            position relative
            top $ px (-20)
            left $ px (-290)
        div # ".usp" ? do
            display flex
            justifyContent center
            maxWidth $ px 1280
            marginLeft auto
            marginRight auto
            padding (px 36) (px 24) (px 36) (px 24)
            div # ".caption" ? do
                fontWeight bold
                width $ px 160
                marginRight $ px 24
            div # ".description" ? do
                minWidth $ px 400
                maxWidth $ pct 40
                fontSize $ pt 14

        div # ".action" ? do
            display flex
            justifyContent center

            div # ".button" ? do
                width $ px 296
                height $ px 90
                alignItems center
                backgroundColor lightgray
                fontSize $ pt 12
                borderRadius (px 24) (px 24) (px 24) (px 24)
                margin (px 4) (px 4) (px 4) (px 4)
                border none (px 0) lightgray
                textAlign center
                display flex
                div ? padding (px 8) (px 8) (px 8) (px 8)

            button ? do
                width $ px 296
                height $ px 90
                display flex
                alignItems center
                backgroundColor lightgray
                borderRadius (px 24) (px 24) (px 24) (px 24)
                margin (px 4) (px 4) (px 4) (px 4)
                border none (px 0) lightgray
                -- border none (px 0) transparent
                cursor pointer
                fontFamily ["Abel"] [sansSerif]
                ":hover" & backgroundColor colorLinkVisited

                div ? margin (px 4) (px 4) (px 4) (px 4)

                div # ".icon" ? do
                    flexGrow 1
                    flexShrink 0
                    flexBasis auto
                    span ? margin (px 1) (px 1) (px 1) (px 1)

                div # ".countrycode" ? do
                    flexGrow 1
                    flexShrink 0
                    flexBasis auto
                    fontSize $ pt 18
                    fontWeight bold
                    color anthrazit

                div # ".description" ? do
                    flexGrow 0
                    flexShrink 1
                    flexBasis auto
                    fontSize $ pt 12
                    color black
                    padding (px 8) (px 8) (px 8) (px 8)

        footer ? do
            borderTop solid (px 1) gray
            padding (px 12) (px 12) (px 12) (px 12)
            fontSize $ pt 12
            color anthrazit
            width $ pct 90

    div # ".taskWords" ? do
        display flex
        alignItems center
        span ? do
            display inlineBlock
            marginLeft $ em 1
            marginRight $ em 1
            width $ px 120

    div # ".taskWords" |> code ? marginRight (em 1)
