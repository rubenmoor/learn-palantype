module Page.Stage3 where

import Client (getDictDE', getDocDEPattern', postRender, request)
import Common.Route (FrontendRoute)
import Control.Monad (unless)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader.Class (MonadReader, ask)
import Data.Functor (($>), (<&>))
import Data.Semigroup (Endo)
import Obelisk.Route (R)
import Obelisk.Route.Frontend (RouteToUrl, routeLink, SetRoute)
import Page.Common (elCongraz, elNotImplemented, elPatterns, loading, taskWords)
import Palantype.Common (Lang (..), Palantype, toDescription)
import Palantype.DE (Pattern (..))
import Reflex.Dom (DomBuilder, EventWriter, MonadHold, PostBuild, Prerender, delay, el, elClass, getPostBuild, never, switchDyn, text, widgetHold, widgetHold_)
import State (stageUrl, Env (..), Navigation (..), State)
import TextShow (TextShow (showt))

exercise ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      Palantype key,
      PostBuild t m,
      Prerender t m,
      SetRoute t (R FrontendRoute) m
    ) =>
    Int ->
    (Lang -> m ()) ->
    Pattern ->
    (Lang -> m ()) ->
    m Navigation
exercise iEx elIntro pat elExplication = do
    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang == DE) elNotImplemented

    el "h1" $ text "Stage 3"
    el "h2" $ text $ toDescription pat
    el "h3" $ text $ "Exercise " <> showt iEx

    elIntro navLang

    ePb <- postRender $ delay 0.1 =<< getPostBuild
    evEDoc <- request $ getDocDEPattern' pat 0 ePb
    widgetHold_ loading $
        evEDoc <&> \case
            Right doc -> elPatterns doc
            Left str ->
                elClass "div" "paragraph small red"
                    $ text
                    $ "Could not load resource: docs: " <> str

    elExplication navLang

    elClass "div" "paragraph" $ text "Type the following words as they appear!"

    evEDict <- request $ getDictDE' pat 0 ePb
    eDone <- fmap switchDyn $ widgetHold (loading $> never) $
        evEDict <&> \case
            Right (mSW, mWSs) -> taskWords mSW mWSs
            Left str ->
                never
                    <$ elClass
                        "div"
                        "paragraph small red"
                        (text $ "Could not load resource: dict: " <> str)

    elCongraz eDone envNavigation
    pure envNavigation

exercise1 ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      Palantype key,
      PostBuild t m,
      Prerender t m,
      RouteToUrl (R FrontendRoute) m,
      SetRoute t (R FrontendRoute) m
    ) =>
    m Navigation
exercise1 =
    exercise
        1
        (\_ -> elClass "div" "paragraph" $
              text
                  "In general, any word in the natural language translates to some \
                  \steno code based on a couple of straightforward substitutions. \
                  \In this exercise, we start with the most common ones."
        )
        PatReplCommon
        (\navLang -> do
            elClass "div" "paragraph" $ do
              text
                  "First of all, note that these patterns are in addition to the \
                  \simple patterns of the previous "
              routeLink (stageUrl navLang "stage_2-4") (text "Exercise 2.4")
              text ". Thus, if you are missing a letter, it might be among the \
                  \simple patterns. Also, note that the minus sign, -, isn't an actual \
                  \steno key. Instead, it's used to distinguish between the left-hand and \
                  \right-hand version of keys that appear twice on the steno keyboard."
              el "code" $ text "-LS"
              text " simply refers to "
              el "code" $ text "L"
              text " and "
              el "code" $ text "S"
              text " of your right hand. "
              el "code" $ text "L-S"
              text " would refer to the "
              el "code" $ text "L"
              text " of your left hand and the "
              el "code" $ text "S"
              text " of your right hand. Finally, in order to refer to "
              el "code" $ text "S"
              text " and "
              el "code" $ text "L"
              text " of your left hand, the code would look like this: "
              el "code" $ text "SL-"
              text ", always obeying the proper order of steno keys."

            elClass "div" "paragraph" $ do
              text
                  "This is a lot to memorize, right from the start. Take your \
                  \time to discover some regularities. E.g. "
              el "em" $ text "k"
              text ", "
              el "em" $ text "ck"
              text ", "
              el "em" $ text "qu"
              text ", and"
              el "em" $ text "x"
              text " all make use of "
              el "code" $ text "GD"
              text " and phonetically they are, indeed, similar. Other \
              \apparently weird rules follow from necessity. E.g. "
              el "em" $ text "sch"
              text " in the onset has the steno code "
              el "code" $ text "SJ"
              text ", which doesn't work for "
              el "em" $ text "schm"
              text " and "
              el "em" $ text "schw"
              text ", because you only have one middle finger on your left hand! \
                   \Luckily, "
              el "code" $ text "SM"
              text " and "
              el "code" $ text "SW"
              text " happen to be quite convenient shorthands, of which we will \
              \learn more about later."
        )

exercise2 ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      Palantype key,
      PostBuild t m,
      Prerender t m,
      SetRoute t (R FrontendRoute) m
    ) =>
    m Navigation
exercise2 =
    exercise
        2
        (\_ -> elClass "div" "paragraph" $
              text
                  "Now you'll learn a nice simplification. When you encounter one of the \
                  \double consonants of the table below in the coda, you will only need one \
                  \steno key to type them. \
                  \The ss is an exception and the reason is simple: unlike the other \
                  \consonants, the s has two keys for your right hand."
        )
        PatDiConsonant
        (\_ -> elClass "div" "paragraph" $
              text
                  "This rule only is about double consonants in the coda. \
                  \Quite often, a double consonant is devided by an ortographic syllable \
                  \and this rule doesn't apply. In case you wonder what german syllable \
                  \ever ends on dd: None of them. This entry is there only for anglicisms."
        )

exercise3 ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      Palantype key,
      PostBuild t m,
      Prerender t m,
      SetRoute t (R FrontendRoute) m
    ) =>
    m Navigation
exercise3 =
    exercise
        3
        (\_ -> elClass "div" "paragraph" $ do
              text
                  "You might have wondered why there is no key for h for your right hand. \
                  \Vowels that are being stretched out, e.g. with an h, are typed using \
                  \one of the stretch keys. You can call "
              el "code" $ text "~"
              el "em" $
                  text
                      "stretch"
              text " or "
              el "em" $
                  text
                      "Dehnung"
              text ". With "
              el "code" $ text "~"
              text
                  " you can stretch any of the vowels of your right hand. In order to \
                  \stretch the vowels of your right hand, you have "
              el "code" $ text "Ü"
              text ", which doubles as a second stretch key."
        )
        PatCodaH
        (\_ -> elClass "div" "paragraph" $ do
              text
                  "Note that the stretch key isn't only for h, but it also turns i into ie. \
                  \Another thing: "
              el "em" $ text "ö"
              text " is typed using two keys, already, and its stretched version "
              el "em" $ text "öh"
              text " isn't typed any differently."
        )

exercise4 ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      Palantype key,
      PostBuild t m,
      Prerender t m,
      RouteToUrl (R FrontendRoute) m,
      SetRoute t (R FrontendRoute) m
    ) =>
    m Navigation
exercise4 =
    exercise
        4
        ( \_ -> elClass "div" "paragraph" $ do
              text "This might come as a little surprise but, just like with "
              el "em" $ text "h"
              text ", the letter "
              el "em" $ text "r"
              text
                  " doesn't have a key for your right hand either and it is \
                  \typed using the same stretch keys."
        )
        PatCodaR
        ( \navLang -> elClass "div" "paragraph" $ do
              text "There is the exception of "
              el "em" $ text "ur"
              text ", which uses "
              el "code" $ text "+"
              text " to leave "
              el "code" $ text "~U"
              text " for the way more frequent "
              el "em" $ text "uhr"
              text ". But about this one, we will learn in "
              routeLink (stageUrl navLang "stage_3-6") (text "Exercise 3.6")
        )

exercise5 ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      Palantype key,
      PostBuild t m,
      Prerender t m,
      SetRoute t (R FrontendRoute) m
    ) =>
    m Navigation
exercise5 =
    exercise
        5
        (\_ -> elClass "div" "paragraph" $ do
            text "Following the idea of stretching vowels, a "
            el "em" $ text "rr"
            text " is yet another way to stretch a vowel and it's typed \
            \using the "
            el "code" $ text "+"
            text " key in addition to the appropriate stretch key."
        )
        PatCodaRR
        (\_ -> elClass "div" "paragraph" $ do
              text "You might have noticed already, how the exercises become \
                   \more difficult as the rules stack up."
        )

exercise6 ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      Palantype key,
      PostBuild t m,
      Prerender t m,
      SetRoute t (R FrontendRoute) m
    ) =>
    m Navigation
exercise6 =
    exercise
        5
        (\_ -> elClass "div" "paragraph" $ do
              text "This is the last rule regarding regular vowel streting and \
                   \it is fairly easy."
        )
        PatCodaHR
        (\_ -> elClass "div" "paragraph" $ do
              text "For the most part, you don't have to care at all that much \
                   \about -h, -r, and -hr as the steno keys are identical."
        )

exercise7 ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      Palantype key,
      PostBuild t m,
      Prerender t m,
      SetRoute t (R FrontendRoute) m
    ) =>
    m Navigation
exercise7 =
    exercise
        5
        (\_ -> elClass "div" "paragraph" $ do
              text "intro"
        )
        PatDt
        (\_ -> elClass "div" "paragraph" $ do
              text "explication"
        )

exercise8 ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      Palantype key,
      PostBuild t m,
      Prerender t m,
      SetRoute t (R FrontendRoute) m
    ) =>
    m Navigation
exercise8 =
    exercise
        5
        (\_ -> elClass "div" "paragraph" $ do
              text "intro"
        )
        PatDiphtong
        (\_ -> elClass "div" "paragraph" $ do
              text "explication"
        )

exercise9 ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      Palantype key,
      PostBuild t m,
      Prerender t m,
      SetRoute t (R FrontendRoute) m
    ) =>
    m Navigation
exercise9 =
    exercise
        5
        (\_ -> elClass "div" "paragraph" $ do
              text "intro"
        )
        PatReplC
        (\_ -> elClass "div" "paragraph" $ do
              text "explication"
        )

exercise10 ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      Palantype key,
      PostBuild t m,
      Prerender t m,
      SetRoute t (R FrontendRoute) m
    ) =>
    m Navigation
exercise10 =
    exercise
        5
        (\_ -> elClass "div" "paragraph" $ do
              text "intro"
        )
        PatCodaGK
        (\_ -> elClass "div" "paragraph" $ do
              text "explication"
        )

exercise11 ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      Palantype key,
      PostBuild t m,
      Prerender t m,
      SetRoute t (R FrontendRoute) m
    ) =>
    m Navigation
exercise11 =
    exercise
        5
        (\_ -> elClass "div" "paragraph" $ do
              text "intro"
        )
        PatSZ
        (\_ -> elClass "div" "paragraph" $ do
              text "explication"
        )

exercise12 ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      Palantype key,
      PostBuild t m,
      Prerender t m,
      SetRoute t (R FrontendRoute) m
    ) =>
    m Navigation
exercise12 =
    exercise
        5
        (\_ -> elClass "div" "paragraph" $ do
              text "intro"
        )
        PatIJ
        (\_ -> elClass "div" "paragraph" $ do
              text "explication"
        )

exercise13 ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      Palantype key,
      PostBuild t m,
      Prerender t m,
      SetRoute t (R FrontendRoute) m
    ) =>
    m Navigation
exercise13 =
    exercise
        5
        (\_ -> elClass "div" "paragraph" $ do
              text "intro"
        )
        PatTsDsPs
        (\_ -> elClass "div" "paragraph" $ do
              text "explication"
        )

exercise14 ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      Palantype key,
      PostBuild t m,
      Prerender t m,
      SetRoute t (R FrontendRoute) m
    ) =>
    m Navigation
exercise14 =
    exercise
        5
        (\_ -> elClass "div" "paragraph" $ do
              text "intro"
        )
        PatDiVowel
        (\_ -> elClass "div" "paragraph" $ do
              text "explication"
        )

exercise15 ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      Palantype key,
      PostBuild t m,
      Prerender t m,
      SetRoute t (R FrontendRoute) m
    ) =>
    m Navigation
exercise15 =
    exercise
        5
        (\_ -> elClass "div" "paragraph" $ do
              text "intro"
        )
        PatReplH
        (\_ -> elClass "div" "paragraph" $ do
              text "explication"
        )

exercise16 ::
    forall key t (m :: * -> *).
    ( DomBuilder t m,
      EventWriter t (Endo State) m,
      MonadFix m,
      MonadHold t m,
      MonadReader (Env t key) m,
      Palantype key,
      PostBuild t m,
      Prerender t m,
      SetRoute t (R FrontendRoute) m
    ) =>
    m Navigation
exercise16 =
    exercise
        5
        (\_ -> elClass "div" "paragraph" $ do
              text "intro"
        )
        PatSmallS
        (\_ -> elClass "div" "paragraph" $ do
              text "explication"
        )
