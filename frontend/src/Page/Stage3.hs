{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Page.Stage3 where

import Client (getDictDE', getDocDEPattern', postRender, request)
import Common.Route (FrontendRoute)
import Common.Stage (Stage, StageMeta (..), stageMeta)
import Control.Category ((<<<))
import Control.Monad (unless)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import Data.Functor (($>), (<&>))
import Data.Semigroup (Endo)
import Obelisk.Route (R)
import Obelisk.Route.Frontend (Routed, RouteToUrl, SetRoute, routeLink)
import Page.Common (getStatsLocalAndRemote, elCongraz, elNotImplemented, elPatterns, loading, taskWords)
import Palantype.Common (Lang (..), Palantype, toDescription)
import Palantype.Common.TH (failure, readLoc)
import Palantype.DE (Pattern (..))
import Reflex.Dom (current, gate, TriggerEvent, DomBuilder, EventWriter, MonadHold, PerformEvent, Performable, PostBuild, Prerender, delay, el, elClass, getPostBuild, never, switchDyn, text, widgetHold, widgetHold_)
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
    , Routed t Stage m
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
exercise iEx elIntro pat elExplication = mdo
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

    dynStats <- getStatsLocalAndRemote evDone
    evEDict <- request $ getDictDE' pat 0 ePb
    evDone <- fmap switchDyn $ widgetHold (loading $> never) $
        evEDict <&> \case
            Right (mSW, mWSs) -> if null mSW
              then do el "p" $
                        text "There are no words in this exercise. \
                          \This is probably an error. \
                          \Skip this for now."
                      pure never
              else taskWords dynStats (gate (not <$> current dynDone) envEChord) mSW mWSs
            Left str -> never <$ elClass
                        "div"
                        "paragraph small red"
                        (text $ "Could not load resource: dict: " <> str)

    dynDone <- elCongraz (Just <$> evDone) dynStats envNavigation
    pure envNavigation

exercise1 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise1 =
    exercise
        1
        ( \_ ->
              el "p" $
                  text
                      "In general, any word in the natural language translates to some \
                      \steno code based on a couple of straightforward substitutions. \
                      \In this exercise, we start with the most common ones."
        )
        PatReplCommon
        ( \navLang -> do
              el "p" $ do
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

              el "p" $ do
                  text
                      "This is a lot to memorize, right from the start. Take your \
                      \time to discover some regularities. E.g. the "
                  el "code" $ text "+"
                  text " turns "
                  el "em" $ text "g"
                  text ", "
                  el "em" $ text "d"
                  text ", and"
                  el "em" $ text "b"
                  text " into "
                  el "em" $ text "k"
                  text ", "
                  el "em" $ text "t"
                  text ", and"
                  el "em" $ text "p"
                  text ", respectively. The "
                  el "em" $ text "x"
                  text " looks weird but it really is simply typed by "
                  el "code" $ text "DSG"
                  text " with the left hand, which becomes "
                  el "code" $ text "GSD"
                  text " with the right hand."
        )

exercise2 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise2 = exercise
   2
   ( \_ -> el "p" $ do
       text "The new rules of this exercise all follow from one single rule: "
       el "em" $ text "t"
       text " is typed by "
       el "code" $ text "+D"
       text ". But there is a lot space between those two keys to squeeze in \
            \another consonant when needed. Exceptions are needed for "
       el "em" $ text "mt"
       text " and "
       el "em" $ text "lt"
       text ", where the "
       el "code" $ text "+"
       text " is omitted at the expense of specificity."
   )
   PatCodaComboT
   ( \_ -> el "p" $ do
       text "TODO: follow-up text"
   )

exercise3 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise3 = exercise
   3
   ( \_ -> el "p" $ do
       text "TODO: introductory text"
   )
   PatOnsetR
   ( \_ -> el "p" $ do
       text "TODO: follow-up text"
   )

exercise4 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise4 = exercise
   4
   ( \_ -> el "p" $ do
       text "TODO: introductory text"
   )
   PatOnsetL
   ( \_ -> el "p" $ do
       text "TODO: follow-up text"
   )

exercise5 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise5 =
    exercise
        5
        ( \_ -> el "p" $ do
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
        ( \_ -> el "p" $ do
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

exercise6 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise6 =
    exercise
        6
        ( \_ ->
              el "p" $
                  text
                      "Now you'll learn a nice simplification. When you encounter one of the \
                      \double consonants of the table below in the coda, you will only need one \
                      \steno key to type them. \
                      \The ss is an exception and the reason is simple: unlike the other \
                      \consonants, the s has two keys for your right hand."
        )
        PatDiConsonant
        ( \_ ->
              el "p" $
                  text
                      "This rule only is about double consonants in the coda. \
                      \Quite often, a double consonant is devided by an ortographic syllable \
                      \and this rule doesn't apply. In case you wonder what german syllable \
                      \ever ends on dd: None of them. This entry is there only for anglicisms."
        )

exercise7 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise7 =
    exercise
        7
        ( \_ -> el "p" $ do
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
        ( \_ -> el "p" $ do
              text
                  "Note that the stretch key isn't only for h, but it also turns i into ie. \
                  \Another thing: "
              el "em" $ text "ö"
              text " is typed using two keys, already, and its stretched version "
              el "em" $ text "öh"
              text " isn't typed any differently."
        )

exercise8 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise8 =
    exercise
        8
        ( \_ -> el "p" $ do
              text "This might come as a little surprise but, just like with "
              el "em" $ text "h"
              text ", the letter "
              el "em" $ text "r"
              text
                  " doesn't have a key for your right hand either and it is \
                  \typed using the same stretch keys."
        )
        PatCodaR
        ( \_ -> el "p" $ do
              text "As in the previous exercise, "
              el "em" $ text "ö"
              text
                  " already uses two keys and its stretched version doesn't \
                  \look any different then the unstretched "
              el "em" $ text "ö"
              text "."
        )

exercise9 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise9 =
    exercise
        9
        ( \_ -> el "p" $ do
              text "Following the idea of stretching vowels, a "
              el "em" $ text "rr"
              text
                  " is yet another way to stretch a vowel and it's typed \
                  \using the "
              el "code" $ text "+"
              text " key."
        )
        PatCodaRR
        ( \lang -> el "p" $ do
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

exercise10 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise10 =
    exercise
        10
        ( \_ ->
              el "p" $
                  text
                      "This is the last rule regarding regular vowel streting and \
                      \it is fairly straightforward."
        )
        PatCodaHR
        ( \_ -> el "p" $ do
              text "Use "
              el "code" $ text "~"
              text " with "
              el "code" $ text "+"
              text " to reach "
              el "em" $ text "hr"
              text "."
        )

exercise11 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise11 =
    exercise
        11
        ( \_ -> do
              el "p" $ do
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
              el "p" $ do
                  text
                      "Luckily, there are not that many words at all that suffer \
                      \from ambiguity regarding -dt and this exercise is quite small. \
                      \A lot of -dt words are typed simply with "
                  el "code" $ text "-D"
                  text " alone and thus don't show up here."
        )
        PatDt
        ( \lang -> el "p" $ do
              text
                  "You can find more information and examples of this \
                  \rule in the correspondig section of the "
              routeLink (stageUrl lang $ $readLoc "patternoverview") $
                  text "pattern overview"
              text "."
        )

exercise12 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise12 =
    exercise
        12
        ( \_ -> el "p" $ do
              text
                  "Here you find combinations of vowels in the nucleus that \
                  \do not conform to the rules of simple, letter-by-letter \
                  \replacement."
        )
        PatDiphtong
        ( \_ -> el "p" $ do
              text
                  "If those seemed weird to you, that's because they are weird. \
                  \Fortunately, the weird rules usually affect rare words."
        )

exercise13 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise13 =
    exercise
        13
        ( \_ -> el "p" $ do
              text "You will learn an alternative to type "
              el "em" $ text "c"
              text " later. To help you memorize this rule, think of "
              el "em" $ text "k"
              text ", which is typed "
              el "code" $ text "GD-"
              text " in the onset."
        )
        PatReplC
        ( \_ -> el "p" $ do
              text "Also, remember that you already learned "
              el "em" $ text "ch"
              text ", which is treated as an entirely different letter and typed "
              el "code" $ text "SHJ"
              text ", just like "
              el "em" $ text "sch"
              text "."
        )

exercise14 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise14 =
    exercise
        14
        ( \_ -> el "p" $ do
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
        ( \_ -> el "p" $ do
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

exercise15 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise15 =
    exercise
        15
        ( \_ -> el "p" $ do
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
        PatBreakUpI
        ( \_ -> el "p" $ do
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

exercise16 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise16 =
    exercise
        16
        ( \_ -> el "p" $ do
              text "intro"
        )
        PatSwapS
        ( \_ -> el "p" $ do
              text "explication"
        )

exercise17 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise17 =
    exercise
        17
        ( \_ -> el "p" $ do
              text "intro"
        )
        PatSwapSch
        ( \_ -> el "p" $ do
              text "explication"
        )

exercise18 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise18 =
    exercise
        18
        ( \_ -> el "p" $ do
              text "intro"
        )
        PatSwapZ
        ( \_ -> el "p" $ do
              text "explication"
        )

exercise19 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise19 =
    exercise
        19
        ( \_ -> el "p" $ do
              text "intro"
        )
        PatDiVowel
        ( \_ -> el "p" $ do
              text "explication"
        )

exercise20 ::
    forall key t (m :: * -> *).
    Constraints key t m =>
    m Navigation
exercise20 =
    exercise
        20
        ( \_ -> el "p" $ do
              text "intro"
        )
        PatReplH
        ( \_ -> el "p" $ do
              text "explication"
        )

exercise21
  :: forall key t (m :: * -> *)
  .  Constraints key t m
  => m Navigation
exercise21 = mdo
    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang == DE) elNotImplemented

    el "h1" $ text "Stage 3"
    el "h2" $ text $ toDescription PatCodaGK
    el "h3" $ text $ "Exercise 21"

    el "p" $ text "intro"

    ePb <- postRender $ delay 0.1 =<< getPostBuild
    evEDoc <- request $ getDocDEPattern' PatCodaGK 3 ePb

    widgetHold_ loading $
        evEDoc <&> \case
            Right doc -> elPatterns doc
            Left str ->
                elClass "div" "paragraph small red"
                    $ text
                    $ "Could not load resource: docs: " <> str

    el "p" $ text "explication"

    dynStats <- getStatsLocalAndRemote evDone
    evEDict <- request $ getDictDE' PatCodaGK 3 ePb
    evDone <- fmap switchDyn $ widgetHold (loading $> never) $
        evEDict <&> \case
            Right (mSW, mWSs) -> if null mSW
              then do el "p" $
                        text "There are no words in this exercise. \
                          \This is probably an error. \
                          \Skip this for now."
                      pure never
              else taskWords dynStats (gate (not <$> current dynDone) envEChord) mSW mWSs
            Left str -> never <$ elClass
                        "div"
                        "paragraph small red"
                        (text $ "Could not load resource: dict: " <> str)

    dynDone <- elCongraz (Just <$> evDone) dynStats envNavigation
    pure envNavigation
