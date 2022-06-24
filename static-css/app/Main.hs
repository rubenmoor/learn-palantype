{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Clay                           (type_, video, h1, cursive, firstChild, caption, textShadow,  (#)
                                                , (&)
                                                , (-:)
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
                                                , alignSide
                                                , animation
                                                , background
                                                , backgroundColor
                                                , before
                                                , black
                                                , block
                                                , blockquote
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
                                                , br
                                                , breakWord
                                                , bsColor
                                                , bsInset
                                                , button
                                                , clear
                                                , code
                                                , collapse
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
                                                , hr
                                                , html
                                                , i
                                                , img
                                                , important
                                                , infinite
                                                , inherit
                                                , inline
                                                , inlineBlock
                                                , inlineFlex
                                                , input
                                                , italic
                                                , justifyContent
                                                , keyframes
                                                , left
                                                , li
                                                , lightgray
                                                , lightgreen
                                                , lineHeight
                                                , linear
                                                , link
                                                , listStyleType
                                                , margin
                                                , marginBottom
                                                , marginLeft
                                                , marginRight
                                                , marginTop
                                                , maxWidth
                                                , middle
                                                , minHeight
                                                , minWidth
                                                , monospace
                                                , nthChild
                                                , opacity
                                                , orange
                                                , outline
                                                , overflow
                                                , overflowClip
                                                , overflowEllipsis
                                                , overflowWrap
                                                , overflowY
                                                , padding
                                                , paddingBottom
                                                , paddingLeft
                                                , paddingRight
                                                , paddingTop
                                                , pct
                                                , pink
                                                , pointer
                                                , position
                                                , preWrap
                                                , pt
                                                , putCss
                                                , px
                                                , queryOnly
                                                , red
                                                , relative
                                                , rem
                                                , rgb
                                                , rgba
                                                , right
                                                , sansSerif
                                                , sec
                                                , section
                                                , shadowWithBlur
                                                , shadowWithSpread
                                                , sideCenter
                                                , sideLeft
                                                , sideRight
                                                , small
                                                , smaller
                                                , solid
                                                , span
                                                , star
                                                , sticky
                                                , stringContent
                                                , strong
                                                , table
                                                , td
                                                , textAlign
                                                , textDecoration
                                                , textOverflow
                                                , th
                                                , top
                                                , tr
                                                , transform
                                                , transition
                                                , translate
                                                , transparent
                                                , ul
                                                , underline
                                                , vAlignBottom
                                                , verticalAlign
                                                , violet
                                                , visibility
                                                , visited
                                                , white
                                                , whiteSpace
                                                , width
                                                , yellow
                                                , zIndex
                                                , (|>)
                                                )
import           Clay                           ( borderTopColor )
import           Clay                           ( borderTopStyle )
import           Clay                           ( borderTopWidth )
import           Clay                           ( nowrap )
import           Clay                           ( borderColor )
import           Clay                           ( borderCollapse )
import           Clay.Border                    ( borderBottom
                                                , borderTop
                                                , dotted
                                                )
import           Clay.Color                     ( darkgray )
import           Clay.Flexbox                   ( row )
import qualified Clay.Flexbox                  as Flex
import qualified Clay.Media                    as Media
import           Control.Applicative            ( Applicative(pure) )
import           Data.Function                  ( ($) )
import           Data.Monoid                    ( Monoid(mempty) )
import           Data.Semigroup                 ( Semigroup((<>)) )
import           GHC.IO                         ( IO )
import           GHC.Num                        ( Num((*), (+), (-), negate) )
import           System.IO                      ( putStrLn )
import Clay (borderLeft)
import Clay (borderRight)
import Clay (lightyellow)
import Clay (flexWrap)
import Clay (wrap)

anthrazit :: Color
anthrazit = rgb 8 20 48 -- #081430;

myLightgray :: Color
myLightgray = rgb 230 230 230

myLightblue :: Color
myLightblue = rgb 185 206 255


myBlue :: Color
myBlue = rgba 13 83 181 0.41

myGray :: Color
myGray = rgb 126 133 167

colorLink :: Color
colorLink = blue

colorLinkVisited :: Color
colorLinkVisited = rgb 0 51 102

main :: IO ()
main = putCss $ do
    html ? height (pct 100)
    body ? do
        fontFamily ["Abel"] [sansSerif]
        fontSize $ pt 18
        margin (px 0) (px 0) (px 0) (px 0)
        a ? do
            link & do
                textDecoration none
                color colorLink
            hover & textDecoration underline
            visited & color colorLinkVisited
            ".normalLink" & do
                color colorLink
                cursor pointer
        height $ pct 100
        h2 ? do
            color darkblue
            marginTop $ px 60
            a # link ? do
                textDecoration none
                color inherit
            a # hover ? textDecoration none
        h3 ? color anthrazit
        h3 |> a # link ? do
            textDecoration none
            color anthrazit
        h3 |> a # hover ? color darkblue
        h3 ? i ? do
            color myLightgray
            paddingLeft $ em 0.5
            paddingRight $ em 0.5
    star ? boxSizing borderBox

    -- flex layout

    div # ".box" ? do
        display flex
        flexFlow column Flex.nowrap
        height $ pct 100

    header ? do
        flexGrow 0
        flexShrink 1
      -- already the default? flexBasis auto

    div # ".row" ? do
        display flex
        flexFlow row Flex.nowrap
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

    -- section # "#content" # before ? do
        -- content $ stringContent "Up \\25B2  DMKSD" -- U+25B2
    section # "#content" |> div # ".scrollTop" ? do
        width $ other "max-content"
        height $ px 29
        position sticky
        top $ px 2
        fontSize $ pt 12
        fontWeight bold
        marginLeft auto
        marginRight auto
        backgroundColor $ rgb 102 141 60
        opacity 0.7
        color white
        borderRadius (px 8) (px 8) (px 8) (px 8)
        padding (px 4) (px 4) (px 4) (px 4)

    section # "#content" |> div # ".content" # before ? do
        content $ stringContent ""
        width $ pct 80
        backgroundColor white
        position absolute
        top $ px 2
        height $ px 29

    section # "#content" |> div # ".scrollBottom" ? do
        width $ other "max-content"
        height $ px 29
        position sticky
        bottom $ px 8
        fontSize $ pt 12
        fontWeight bold
        marginLeft auto
        marginRight auto
        backgroundColor $ rgb 102 141 60
        opacity 0.7
        color white
        borderRadius (px 8) (px 8) (px 8) (px 8)
        padding (px 4) (px 4) (px 4) (px 4)

    section # "#content" |> div # ".content" # after ? do
        content $ stringContent ""
        width $ pct 80
        backgroundColor white
        position absolute
        height $ px 370
        zIndex 1

    section # ".toc" ? do
        marginTop $ em 0.83
        paddingLeft $ px 12
        paddingTop $ px 12
        flexShrink 0
        overflowY auto
        div ? marginRight (px 12)
        ul ? do
            listStyleType none
            margin (px 0) (px 0) (px 0) (px 0)
            padding (px 0) (px 0) (px 0) (px 0)
            li ? do
                lineHeight $ em 1.8
                whiteSpace nowrap
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
        maxWidth $ px 518
        textAlign center
        backgroundColor white
        boxShadow
            [ bsColor (rgba 0 0 0 0.2)
                  $ shadowWithSpread (px 0) (px 8) (px 16) (px 0)
            ]
        padding (px 24) (px 24) (px 24) (px 24)
        position fixed
        zIndex 2

    div # ".msgOverlay" ? do
        bottom $ em 1
        left $ pct 50
        transform (translate (pct $ -50) $ pct 0)
        maxWidth $ px 418
        borderRadius (px 12) (px 12) (px 12) (px 12)
        backgroundColor anthrazit
        color myLightgray
        fontSize (pt 12)
        padding (px 12) (px 12) (px 12) (px 12)
        position fixed
        zIndex 3
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
        input # "#stenoOutput" ? do
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

        div # ".configuration" ? do
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
                    div # ".numberMode" ? visibility hidden
                    div # ".numberModeShift" ? visibility hidden
                ".inactive.pressed" & boxShadow (pure none)
                ".numberMode" & do
                    div # ".steno" ? display none
                    div # ".numberMode" ? display block
                    div # ".numberModeShift" ? display none
                ".numberModeShift" & do
                    div # ".steno" ? display none
                    div # ".numberMode" ? display none
                    div # ".numberModeShift" ? display block
                ".numberMode.small" & div # ".numberMode" ? fontSize (pt 14)
                ".numberMode.verySmall" & div # ".numberMode" ? fontSize (pt 8)
                ".numberModeShift.small" & div # ".numberModeShift" ? fontSize
                    (pt 14)
                ".numberModeShift.verySmall"
                    & div
                    # ".numberModeShift"
                    ? fontSize (pt 8)

                div ? do
                    ".steno" & do
                        fontWeight bold
                        fontSize $ pt 18
                    ".qwerty" & do
                        fontSize $ pt 10
                        color gray
                        marginTop $ px (-6)
                    ".numberMode" & do
                        fontWeight bold
                        fontSize $ pt 18
                        display none
                        height $ rem 2
                    ".numberModeShift" & do
                        fontWeight bold
                        fontSize $ pt 18
                        display none
                        height $ rem 2

    div # ".topmenu" ? do
        boxShadow
          [ bsColor (rgba 0 0 0 0.2)
            $ shadowWithSpread (px 0) (px 2) (px 8) (px 0)
          ]
        marginBottom $ em 0.5
        padding (em 0.2) (em 0.2) (em 0.2) (em 0.2)

        div # ".topmenu-entry" ? do
          paddingLeft $ em 0.5
          paddingRight $ em 0.5

        span # ".vertical-line" ? do
          -- display inlineBlock
          borderLeft solid (px 1) lightgray

        div # ".login-signup" ? do
          float floatRight
          fontSize $ pt 14
          paddingTop $ em 0.3
          paddingRight $ em 0.5

    div # ".btnToggleKeyboard" ? do
        cursor pointer
        color gray
        display inlineFlex
        code ? do
            fontSize (pt 10)
            padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)
        i ? fontSize (pt 22)
        padding (px 0) (px 2) (px 0) (px 2)
        ".keyboardVisible" & color darkblue
        hover & do
            color darkblue
            borderColor darkblue

    span # ".btn" ? do
        color gray
        fontSize $ pt 24

    span # ".checkmark" ? do
        width (em 1)
        display inlineBlock

    div # ".dropdown" ? do

        position relative
        display inlineBlock

        span # ".dropdown-button" ? do
            color gray
            fontSize $ pt 24
            cursor pointer
            hover & color darkblue
            fontFamily ["Special Elite"] [cursive]

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

            span # ".entry" ? do
                minWidth $ px 160
                padding (px 12) (px 12) (px 12) (px 12)
                display block
                hover & backgroundColor lightgray

            span # ".caption" ? do
                padding (px 12) (px 12) (px 0) (px 12)
                display block
                color anthrazit
                fontSize $ pt 10
                fontWeight bold
                borderBottom solid (px 1) lightgray

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
        backgroundColor myLightblue
        padding (px 36) (px 36) (px 36) (px 36)

    ".bgWhite" ? backgroundColor white
    ".bgGreen" ? backgroundColor lightgreen
    ".bgRed" ? backgroundColor red
    ".bgPink" ? backgroundColor pink
    ".bgLightgreen" ? backgroundColor lightgreen
    ".bgLightblue" ? backgroundColor myLightblue
    ".bgLightgray" ? backgroundColor myLightgray
    ".fgTransparent" ? color transparent
    ".red" ? color red
    ".small" ? fontSize (pt 12)
    ".verySmall" ? fontSize (pt 10)
    ".anthrazit" ? color anthrazit
    ".darkgray" ? color darkgray

    ".exerciseField" ? do
        backgroundColor myLightgray
        borderRadius (px 4) (px 4) (px 4) (px 4)
        width $ other "fit-content"
        padding (px 6) (px 6) (px 6) (px 6)
        span ? padding (px 2) (px 2) (px 2) (px 2)
        marginBottom $ em 0.5
    div # ".exerciseField.multiline" ? do
        width $ pct 100
        overflowWrap breakWord
        lineHeight $ em 1.8

    div # ".congraz" ? do
        textAlign center
        div # ".check" ? i ? do
            fontSize $ pt 72
            color green
            padding (px 32) (px 32) (px 32) (px 32)
        div # ".newBest" ? i ? do
            color yellow
            paddingLeft $ em 1
            paddingRight $ em 1
            textShadow (px 1) (px 1) (px 5) gray
        table # ".statistics" ? tr # firstChild ? backgroundColor lightyellow

    div # ".paragraph" ? marginBottom (em 0.5)
    div # ".paragraph" |> code ? fontSize (pt 12)

    ".floatLeft" ? float floatLeft
    ".floatRight" ? float floatRight
    ".clearBoth" ? clear both

    div # ".start" ? do
        textAlign center
        margin (px 24) (px 24) (px 24) (px 24)

    hr ? do
        border none (px 0) black
        height $ px 1
        backgroundColor myBlue

    ".displayNone" ? display none
    ".displayFlex" ? display flex
    ".visibilityHidden" ? visibility hidden
    ".block" ? display block
    ".inline" ? display inline
    ".inlineBlock" ? display inlineBlock
    ".fontSizeSmaller" ? fontSizeCustom smaller

    ".blinking" ? animation "blink" (sec 1) ease (sec 0) infinite normal none
    ".lightgray" ? color myLightgray

    keyframes "blink" [(0, opacity 0), (0.5, opacity 1), (1, opacity 0)]

    div # ".landing" ? do
        backgroundColor myGray
        div # ".top" ? do
            width $ pct 100
            height $ px 160
            h1 ? do
                fontFamily ["Special Elite"] [cursive]
                display inlineBlock
                marginTop $ em 1
                marginLeft $ em 1
                fontWeight normal
        div # ".middle" ? do
            width $ pct 100
            backgroundColor myLightblue
            textAlign center
            paddingTop $ em 4
            paddingBottom $ em 4
            paddingLeft $ em 2
            paddingRight $ em 2
            h1 ? do
                color white
                marginBottom $ em 1.5

        div # ".middle" |> div # ".container" ? do
            display flex
            flexWrap Flex.wrap
            alignItems center
            justifyContent center
            video ? do
                backgroundColor white
                margin (px 4) (px 4) (px 4) (px 4)
                border solid (px 8) white

            div # ".right" ? do
                display flex
                flexWrap Flex.wrap
                justifyContent center

            div # ".action" ? do
                button ? do
                    height (px 198)
                    width $ px 360
                    backgroundColor colorLinkVisited
                    color white
                marginLeft $ px 36
                marginRight $ px 36

            div # ".button" ? do
                width $ px 296
                height $ px 90
                alignItems center
                fontSize $ pt 12
                borderRadius (px 24) (px 24) (px 24) (px 24)
                margin (px 4) (px 4) (px 4) (px 4)
                border solid (px 1) black
                textAlign center
                display flex
                color anthrazit
                div ? padding (px 8) (px 8) (px 8) (px 8)

            button |> div # ".container" ? do
                display flex
                alignItems center

            button ? do
                backgroundColor myBlue
                borderRadius (px 24) (px 24) (px 24) (px 24)
                margin (px 4) (px 4) (px 4) (px 4)
                border none (px 0) lightgray
                cursor pointer
                fontFamily ["Abel"] [sansSerif]

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

                div # ".description" ? do
                    flexGrow 0
                    flexShrink 1
                    flexBasis auto
                    fontSize $ pt 12
                    padding (px 8) (px 8) (px 8) (px 8)
                    color white

                div # ".cta" ? do
                    fontSize $ pt 42
                    fontWeight bold

            div # ".other" ? do
                button ? do
                    height (px 100)
                    width $ px 296


        div # ".bottom" ? do
            display flex
            flexWrap Flex.wrap
            justifyContent center

            div # ".usp" ? do
                maxWidth $ px 300
                paddingLeft $ px 30
                paddingRight $ px 30
                marginTop $ em 4
                marginBottom $ em 4
                color white
                div # ".icon" ? do
                    textAlign center
                    height $ px 120
                    i ? do
                        fontSize $ pt 72
                        color anthrazit
                div # ".caption" ? do
                    fontWeight bold
                    fontStyle italic
                    borderBottom solid (px 1) white
                    marginBottom $ em 1
                    textAlign center
                div # ".description" ? do
                    fontSize $ pt 14

        footer ? do
            paddingTop $ em 1
            paddingBottom $ em 1
            color anthrazit
            width $ pct 100
            textAlign center
            backgroundColor white

    div # ".taskSingletons" ? do
        padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)

    div # ".taskSingletons" |> code ? do
        marginLeft  $ em 1
        marginRight $ em 1

    div # ".taskWords" ? do
        padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)

    div # ".taskWords" |> span ? do
        marginRight $ em 1

    div # ".taskWords" |> span # ".word" ? minWidth (px 120)

    span # ".button-submit" ? do
        fontSize $ pt 12
        fontWeight bold
        backgroundColor $ rgb 102 141 60
        color white
        borderRadius (px 12) (px 12) (px 12) (px 12)
        padding (px 6) (px 6) (px 6) (px 6)
        display inlineBlock

    div # ".patternTable" ? do
        marginTop $ em 1.5
        marginBottom $ em 2
        div # ".orig" ? do
            backgroundColor myLightgray
            paddingRight $ em 0.5
            width $ px 100
            height $ px 31
            textAlign $ alignSide sideRight
            display inlineBlock
            verticalAlign vAlignBottom
        code # ".steno" ? do
            width $ px 100
            paddingLeft $ em 0.5
            textAlign $ alignSide sideLeft
            display inlineBlock
            fontSize $ pt 14
        hr # ".Onset" ? do
          backgroundColor pink
          marginBottom $ px $ -20
        hr # ".Nucleus" ? do
          backgroundColor lightgreen
          marginBottom $ px $ -20
        hr # ".Coda" ? do
          backgroundColor myLightblue
          marginBottom $ px $ -20
        hr # ".Multiple" ? do
          backgroundColor violet
          marginBottom $ px $ -20
        hr # ".OnsetAndCoda" ? do
          backgroundColor orange
          marginBottom $ px $ -20
        span # ".patternPosition" ? do
            float floatRight
            fontSize $ px 14
            fontWeight bold
            position relative
            top $ px 20
        span # ".Onset" ? color pink
        span # ".Nucleus" ? color lightgreen
        span # ".Coda" ? color myLightblue
        span # ".Multiple" ? color violet
        span # ".OnsetAndCoda" ? color orange

    div # ".patternExamples" ? do
        backgroundColor myLightblue
        fontSize $ pt 12
        padding (px 8) (px 8) (px 8) (px 8)
        borderRadius (px 8) (px 8) (px 8) (px 8)
        strong ? marginBottom (em 0.5)

        div ? do
            overflow hidden
            textOverflow overflowEllipsis

        button ? do
            backgroundColor transparent
            border none (px 0) black
            cursor pointer
            color anthrazit
            fontSize $ em 0.5

    ".whiteSpaceNoWrap" ? whiteSpace nowrap

    div # ".embeddedToc" ? do
        border solid (px 1) anthrazit
        borderRadius (px 4) (px 4) (px 4) (px 4)
        fontSize $ pt 12
        padding (px 8) (px 8) (px 8) (px 8)
        ul ? do
            listStyleType none
            paddingLeft $ px 0

    table # ".ploverCommands" ? do
        marginTop $ rem 1.5
        marginBottom $ rem 1.5
        borderCollapse collapse
        td <> th ? do
            padding (px 6) (px 6) (px 6) (px 6)
        td # ".plover" ? do
            fontSize $ pt 10
        tr # nthChild "even" ? do
            backgroundColor myLightblue

    table # "#punctuation" ? do
        td # nthChild "1" ? textAlign (alignSide sideCenter)
        td # nthChild "2" ? textAlign (alignSide sideCenter)

    table # "#openingClosing" ? do
        td # nthChild "1" ? textAlign (alignSide sideCenter)
        td # nthChild "3" ? textAlign (alignSide sideCenter)
        td # nthChild "4" ? textAlign (alignSide sideCenter)

    table # "#asciiArt" ? do
        td # nthChild "2" ? fontSize (pt 12)

    span # ".stopwatch" ? do
        fontSize $ pt 14
        color gray
        fontFamily ["DejaVu Sans Mono"] [monospace]
        paddingLeft  $ em 1.5
        paddingRight $ em 1.5

    div # ".stopwatch" ? do
        hr ? do
            borderBottom none (px 0) black
            borderLeft none (px 0) black
            borderRight none (px 0) black
            borderTop dotted (px 1) lightgray
            height $ px 1
        div ? do
            marginTop $ em 0.5
            padding (px 8) (px 8) (px 8) (px 8)
            borderRadius (px 8) (px 8) (px 8) (px 8)
            backgroundColor myLightblue
            width $ other "fit-content"
    table # ".statistics" ? do
        borderCollapse collapse
        fontSize $ pt 14
        lineHeight $ em 1.5
        marginLeft auto
        marginRight auto
        caption ? do
            textAlign $ alignSide sideLeft
            borderBottom solid (px 1) lightgray
        td # ".date" ? do
            color gray
            fontStyle italic
        td # ".time" ? do
            paddingLeft $ em 1
            paddingRight $ em 1
            fontFamily ["DejaVu Sans Mono"] [monospace]
        td # ".nMistakes" ? do
            paddingRight $ em 1

    div # ".auth" ? do
        paddingTop $ em 2
        paddingBottom $ em 2
        maxWidth $ px 1024
        marginLeft auto
        marginRight auto

        input ? do
          fontSize $ pt 18
          borderRadius (px 12) (px 12) (px 12) (px 12)
          padding (px 8) (px 8) (px 8) (px 8)
          outline none (px 0) transparent
          border solid (px 1) myBlue

          -- really only meant for checkboxes:
          -- input[type=checkbox] { ... }
          -- `type_ :: Refinement` exists, but no plan
          marginRight $ em 0.5
        input # focus ? do
          boxShadow
              [ bsColor myBlue
                    $ shadowWithSpread (px 0) (px 0) (px 5) (px 0)
              ]

        button ? do
            backgroundColor anthrazit
            color white
            fontSize $ pt 24
            borderRadius (px 12) (px 12) (px 12) (px 12)
            padding (px 12) (px 12) (px 12) (px 12)
            border none (px 0) transparent
            cursor pointer
