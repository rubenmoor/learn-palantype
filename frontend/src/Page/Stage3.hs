{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Page.Stage3 where

import Client (getDictDE', getDocDEPattern', postRender, request)
import Common.Route (FrontendRoute)
import Common.Stage (StageMeta (..), stageMeta)
import Control.Category ((<<<))
import Control.Monad (unless)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import Data.Functor (($>), (<&>))
import Data.Semigroup (Endo)
import Obelisk.Route (R)
import Obelisk.Route.Frontend (RouteToUrl, SetRoute, routeLink)
import Page.Common (elCongraz, elNotImplemented, elPatterns, loading, taskWords)
import Palantype.Common (Lang (..), Palantype, toDescription)
import Palantype.Common.TH (failure, readLoc)
import Palantype.DE (Pattern (..))
import Reflex.Dom (TriggerEvent, DomBuilder, EventWriter, MonadHold, PerformEvent, Performable, PostBuild, Prerender, delay, el, elClass, getPostBuild, never, switchDyn, text, widgetHold, widgetHold_)
import State (Env (..), Navigation (..), State, stageUrl)
import Text.Read (readMaybe)
import TextShow (TextShow (showt))

type Constraints key t m =
    ( DomBuilder t m
    , EventWriter t (Endo State) m
    , MonadFix m
    , MonadHold t m
    , MonadReader (Env t key) m
    , Palantype key
    , MonadIO (Performable m)
    , PerformEvent t m
    , PostBuild t m
    , Prerender t m
    , RouteToUrl (R FrontendRoute) m
    , SetRoute t (R FrontendRoute) m
    , TriggerEvent t m
    )

exercise
  :: forall key t (m :: * -> *)
  .  Constraints key t m
  => Int
  -> (Lang -> m ())
  -> Pattern
  -> (Lang -> m ())
  -> m Navigation
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

    evEDict <- request $ getDictDE' pat 0 ePb
    evDone <- fmap switchDyn $ widgetHold (loading $> never) $
        evEDict <&> \case
            Right (mSW, mWSs) -> taskWords mSW mWSs
            Left str -> never <$ elClass
                        "div"
                        "paragraph small red"
                        (text $ "Could not load resource: dict: " <> str)

    elCongraz (Just <$> evDone) envNavigation
    pure envNavigation

exercise1 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise1 =
    exercise
        1
        ( \_ ->
              elClass "div" "paragraph" $
                  text
                      "In general, any word in the natural language translates to some \
                      \steno code based on a couple of straightforward substitutions. \
                      \In this exercise, we start with the most common ones."
        )
        PatReplCommon
        ( \navLang -> do
              elClass "div" "paragraph" $ do
                  text
                      "First of all, note that these patterns are in addition to the \
                      \simple patterns of the previous "
                  let stageSimplePatterns = $readLoc "stage_PatSimple_0"
                      (iS, iE) = case stageMeta stageSimplePatterns of
                          StageSubLevel jS jE _ -> (jS, jE)
                          StageTopLevel {} -> $failure "StageSubLebel expected"
                  routeLink (stageUrl navLang stageSimplePatterns)
                      $ text
                      $ "Exercise " <> showt iS <> "." <> showt iE
                  text
                      ". Thus, if you are missing a letter, it might be among the \
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
                  text
                      " and phonetically they are, indeed, similar. Other \
                      \apparently weird rules follow from necessity. E.g. "
                  el "em" $ text "sch"
                  text " in the onset has the steno code "
                  el "code" $ text "SJ"
                  text ", which doesn't work for "
                  el "em" $ text "schm"
                  text " and "
                  el "em" $ text "schw"
                  text
                      ", because you only have one middle finger on your left hand! \
                      \Luckily, "
                  el "code" $ text "SM"
                  text " and "
                  el "code" $ text "SW"
                  text
                      " happen to be quite convenient shorthands, of which we will \
                      \learn more about later."
        )

exercise2 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise2 =
    exercise
        2
        ( \_ -> elClass "div" "paragraph" $ do
              text "For the "
              el "code" $ text "F"
              text " and the "
              el "code" $ text "ʃ"
              text
                  " key, the s in the coda is out of reach. Luckily we can use \
                  \the (small) "
              el "code" $ text "s"
              text " key in that case."
        )
        PatSmallS
        ( \_ -> elClass "div" "paragraph" $ do
              text
                  "This extra key will give us some flexibility when dealing \
                  \with "
              el "em" $ text "-st"
              text ", "
              el "em" $ text "-ds"
              text ", and even "
              el "em" $ text "-tzt"
              text "."
        )

exercise3 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise3 =
    exercise
        3
        ( \_ ->
              elClass "div" "paragraph" $
                  text
                      "Now you'll learn a nice simplification. When you encounter one of the \
                      \double consonants of the table below in the coda, you will only need one \
                      \steno key to type them. \
                      \The ss is an exception and the reason is simple: unlike the other \
                      \consonants, the s has two keys for your right hand."
        )
        PatDiConsonant
        ( \_ ->
              elClass "div" "paragraph" $
                  text
                      "This rule only is about double consonants in the coda. \
                      \Quite often, a double consonant is devided by an ortographic syllable \
                      \and this rule doesn't apply. In case you wonder what german syllable \
                      \ever ends on dd: None of them. This entry is there only for anglicisms."
        )

exercise4 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise4 =
    exercise
        4
        ( \_ -> elClass "div" "paragraph" $ do
              text
                  "You might have wondered why there is no key for h for your right hand. \
                  \Vowels that are being stretched out, e.g. with an h, are typed using \
                  \one of the stretch keys. You can call "
              el "code" $ text "~"
              el "em" $ text "stretch"
              text " or "
              el "em" $ text "Dehnung"
              text ". With "
              el "code" $ text "~"
              text
                  " you can stretch any of the vowels of your right hand. In order to \
                  \stretch the vowels of your right hand, you have "
              el "code" $ text "Ü"
              text ", which doubles as a second stretch key."
        )
        PatCodaH
        ( \_ -> elClass "div" "paragraph" $ do
              text
                  "Note that the stretch key isn't only for h, but it also turns i into ie. \
                  \Another thing: "
              el "em" $ text "ö"
              text " is typed using two keys, already, and its stretched version "
              el "em" $ text "öh"
              text " isn't typed any differently."
        )

exercise5 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise5 =
    exercise
        5
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
        ( \_ -> elClass "div" "paragraph" $ do
              text "As in the previous exercise, "
              el "em" $ text "ö"
              text
                  " already uses two keys and its stretched version doesn't \
                  \look any different then the unstretched "
              el "em" $ text "ö"
              text "."
        )

exercise6 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise6 =
    exercise
        6
        ( \_ -> elClass "div" "paragraph" $ do
              text "Following the idea of stretching vowels, a "
              el "em" $ text "rr"
              text
                  " is yet another way to stretch a vowel and it's typed \
                  \using the "
              el "code" $ text "+"
              text " key."
        )
        PatCodaRR
        ( \lang -> elClass "div" "paragraph" $ do
              text "Do not let the appereance of "
              el "em" $ text "t"
              text
                  " in the patterns disturb you. There is a simple reason: \
                  \You have learned to type "
              el "em" $ text "t"
              text " using "
              el "code" $ text "+D"
              text " in "
              let stageReplCommon0 = $readLoc "stage_PatReplCommon_0"
                  (iS, iE) = case stageMeta stageReplCommon0 of
                      StageSubLevel jS jE _ -> (jS, jE)
                      StageTopLevel {} -> $failure "StageSubLevel expected"
              routeLink (stageUrl lang stageReplCommon0)
                  $ text
                  $ "Exercise " <> showt iS <> "." <> showt iE
              text ". Naively following the rule of reaching "
              el "em" $ text "rr"
              text " using "
              el "code" $ text "+"
              text " would require the "
              el "code" $ text "+"
              text " key twice for "
              el "em" $ text "rrt"
              text
                  ", which is impossible, at least without some explicit rule. \
                  \You might also notice that there is no way to distinctly type "
              el "em" $ text "rrd"
              text ", which luckily does not exist in the German language."
        )

exercise7 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise7 =
    exercise
        7
        ( \_ ->
              elClass "div" "paragraph" $
                  text
                      "This is the last rule regarding regular vowel streting and \
                      \it is fairly straightforward."
        )
        PatCodaHR
        ( \_ -> elClass "div" "paragraph" $ do
              text "Use "
              el "code" $ text "~"
              text " with "
              el "code" $ text "+"
              text " to reach "
              el "em" $ text "hr"
              text "."
        )

exercise8 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise8 =
    exercise
        8
        ( \_ -> do
              elClass "div" "paragraph" $ do
                  text
                      "Now for something a bit different. \
                      \A word part that ends with -dt, requires an additional stroke. \
                      \The main reason for this is the fact that in German, \
                      \there exists both, -dt and -tt next to regular -t. \
                      \The idea behind this cumbersome extrastroke is, to have a \
                      \distinct way of writing -dt, when the same word with -t exists, \
                      \too. E.g. "
                  el "em" $ text "Brand"
                  text " and "
                  el "em" $ text "Brandt"
                  text "."
              elClass "div" "paragraph" $ do
                  text
                      "Luckily, there are not that many words at all that suffer \
                      \from ambiguity regarding -dt and this exercise is quite small. \
                      \A lot of -dt words are typed simply with "
                  el "code" $ text "-D"
                  text " alone and thus don't show up here."
        )
        PatDt
        ( \lang -> elClass "div" "paragraph" $ do
              text
                  "You can find more information and examples of this \
                  \rule in the correspondig section of the "
              routeLink (stageUrl lang $ $readLoc "patternoverview") $
                  text "pattern overview"
              text "."
        )

exercise9 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise9 =
    exercise
        5
        ( \_ -> elClass "div" "paragraph" $ do
              text
                  "Here you find combinations of vowels in the nucleus that \
                  \do not conform to the rules of simple, letter-by-letter \
                  \replacement."
        )
        PatDiphtong
        ( \_ -> elClass "div" "paragraph" $ do
              text
                  "If those seemed weird to you, that's because they are weird. \
                  \Fortunately, the weird rules usually affect rare words."
        )

exercise10 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise10 =
    exercise
        5
        ( \_ -> elClass "div" "paragraph" $ do
              text "You will learn an alternative to type "
              el "em" $ text "c"
              text " later. To help you memorize this rule, think of "
              el "em" $ text "k"
              text ", which is typed "
              el "code" $ text "GD-"
              text " in the onset."
        )
        PatReplC
        ( \_ -> elClass "div" "paragraph" $ do
              text "Also, remember that you already learned "
              el "em" $ text "ch"
              text ", which is treated as an entirely different letter and typed "
              el "code" $ text "SHJ"
              text ", just like "
              el "em" $ text "sch"
              text "."
        )

exercise11 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise11 =
    exercise
        5
        ( \_ -> do
              elClass "div" "paragraph" $ text "TODO"
              elClass "div" "paragraph" $ do
                  text
                      "You learned the stretch key to type -h, -r, and -hr. \
                      \Now you learn to use the stretch key to use "
                  el "code" $ text "-K"
                  text " to type "
                  el "em" $ text "g"
                  text "."
        )
        PatCodaGK
        ( \_ -> elClass "div" "paragraph" $ do
              text "... with the exception of "
              el "em" $ text "-ig"
              text
                  ", the idea being that the stretch key is used for \
                  \vowels that are actually long."
        )

exercise12 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise12 =
    exercise
        5
        ( \_ -> elClass "div" "paragraph" $ do
              text "The main takeaway regarding the German letter "
              el "em" $ text "ß"
              text ": Use "
              el "code" $ text "+S"
              text
                  " in order to type it. There is another aspect you will \
                  \note, however. Any vowel that is followed by "
              el "em" $ text "ß"
              text " is stretched, e.g. "
              el "em" $ text "eß"
              text " becomes "
              el "code" $ text "EÜ+S"
              text " in steno code."
        )
        PatSZ
        ( \_ -> elClass "div" "paragraph" $ do
              text "Also, "
              el "em" $ text "ß"
              text " in the onset usually is simply "
              el "code" $ text "S"
              text ", without "
              el "code" $ text "+"
              text ", as you will learn later. The code "
              el "code" $ text "GFW"
              text
                  " is there just in case. It is used in fingerspelling, for \
                  \example."
        )

exercise13 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise13 =
    exercise
        5
        ( \_ -> elClass "div" "paragraph" $ do
              text
                  "This rule increase efficiency for vowel-heavy words. \
                  \Using "
              el "code" $ text "J"
              text " for "
              el "em" $ text "i"
              text
                  " saves you from breaking up a word and adding an extra chord \
                  \just for one vowel."
        )
        PatIJ
        ( \_ -> elClass "div" "paragraph" $ do
              text "In order to reach "
              el "em" $ text "lio"
              text " and "
              el "em" $ text "lia"
              text " you need to mentally swap the "
              el "em" $ text "l"
              text " and the "
              el "em" $ text "i"
              text
                  ". Those swaps are used sometimes to increase efficiency. \
                  \They are the explicit exceptions to what we called the \
                  \\"Steno Order\" earlier."
        )

exercise14 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise14 =
    exercise
        5
        ( \_ -> elClass "div" "paragraph" $ do
              text "intro"
        )
        PatSwapS
        ( \_ -> elClass "div" "paragraph" $ do
              text "explication"
        )

exercise15 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise15 =
    exercise
        5
        ( \_ -> elClass "div" "paragraph" $ do
              text "intro"
        )
        PatSwapSch
        ( \_ -> elClass "div" "paragraph" $ do
              text "explication"
        )

exercise16 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise16 =
    exercise
        5
        ( \_ -> elClass "div" "paragraph" $ do
              text "intro"
        )
        PatSwapZ
        ( \_ -> elClass "div" "paragraph" $ do
              text "explication"
        )

exercise17 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise17 =
    exercise
        5
        ( \_ -> elClass "div" "paragraph" $ do
              text "intro"
        )
        PatDiVowel
        ( \_ -> elClass "div" "paragraph" $ do
              text "explication"
        )

exercise18 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise18 =
    exercise
        5
        ( \_ -> elClass "div" "paragraph" $ do
              text "intro"
        )
        PatReplH
        ( \_ -> elClass "div" "paragraph" $ do
              text "explication"
        )
