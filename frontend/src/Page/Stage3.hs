{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Page.Stage3 where

import           Common.Stage                   ( StageMeta(..)
                                                , stageMeta
                                                )
import           Control.Category               ( (<<<) )
import           Obelisk.Route.Frontend         ( routeLink
                                                )
import Page.Common.Exercise (Constraints, exercise)
import           Palantype.Common.TH            ( failure
                                                , readLoc
                                                )
import           Palantype.DE                   ( Pattern(..) )
import           Reflex.Dom                     ( el
                                                , text
                                                )
import           State                          (stageUrl,  Navigation(..)
                                                )
import           Text.Read                      ( readMaybe )
import           TextShow                       ( TextShow(showt) )

exercise1 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
exercise1 = exercise
    3 1
    (\_ ->
        el "p"
            $ text
                  "In general, any word in the natural language translates to some \
                      \steno code based on a couple of straightforward substitutions. \
                      \In this exercise, we start with the most common ones."
    )
    PatReplCommon1 0
    (\navLang -> do
        el "p" $ do
            text
                "First of all, note that these patterns are in addition to the \
                      \simple patterns of the previous "
            let stageSimplePatterns = $readLoc "stage_PatSimple_0"
                (iS, iE)            = case stageMeta stageSimplePatterns of
                    StageSubLevel jS jE _ -> (jS, jE)
                    StageTopLevel{}       -> $failure "StageSubLebel expected"
            routeLink (stageUrl navLang stageSimplePatterns)
                $  text
                $  "Exercise "
                <> showt iS
                <> "."
                <> showt iE
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

exercise2 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
exercise2 = exercise
    3 2
    (\_ -> el
        "p"
        do
            text
                "The rules for the replacement of common letters are somewhat \
            \arbitrarily split in two. This is the second half."
    )
    PatReplCommon2 0
    (\lang -> do
        el
            "p"
            do
                text "The idea behind "
                el "em" $ text "phr"
                text " is to combine "
                el "em" $ text "f"
                text " (the pronounciation of "
                el "em" $ text "ph"
                text ") with "
                el "em" $ text "r"
                text ". The "
                el "em" $ text "schw-"
                text " rule is indeed arbitrary and results from the fact that "
                el "code" $ text "ʃ"
                text " and "
                el "code" $ text "F"
                text
                    " cannot be reached at the same time. The same is true for "
                el "em" $ text "zw-"
                text "."

        el
            "p"
            do
                text "The "
                el "em" $ text "sp"
                text " rule is a simplification of "
                el "code" $ text "SB+"
                text ", which is possible because "
                el "em" $ text "sb"
                text " isn't really a thing in German. The "
                el "em" $ text "st"
                text
                    " rule is the first example of a swap—in combination with the fact \
              \that "
                el "em" $ text "sd"
                text " doesn't exist in German. The onset "
                el "em" $ text "y"
                text " is encoded in its use as a consonant here, like in "
                el "em" $ text "Yoghurt"
                text
                    ". Note the different code for its vowel user in the nucleus. \
              \This onset code is also used for "

                let stageFingerspelling = $readLoc "stage_fingerspelling"
                    (iS, iE)            = case stageMeta stageFingerspelling of
                        StageSubLevel jS jE _ -> (jS, jE)
                        StageTopLevel{} -> $failure "StageSubLebel expected"
                routeLink (stageUrl lang stageFingerspelling)
                    $  text
                    $  "Exercise "
                    <> showt iS
                    <> "."
                    <> showt iE
                    <> ": Fingerspelling "

                text ", i.e. to type just the letter Y."

        el
            "p"
            do
                text
                    "For the coda, you find a lot of cases where somewhat arbitrary \
              \rules follow from necessity. Consonant combinations in the coda \
              \are just too plentiful in German."
    )

exercise3 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
exercise3 = exercise
    3 3
    (\_ -> el "p" $ do
        text "The new rules of this exercise all follow from one single rule: "
        el "em" $ text "t"
        text " is typed by "
        el "code" $ text "+D"
        text
            ". But there is a lot space between those two keys to squeeze in \
            \another consonant when needed."
    )
    PatCodaComboT 0
    (\_ -> el "p" $ do
        text
            "This is not the whole story, though. First, exceptions are needed for "
        el "em" $ text "mt"
        text " and "
        el "em" $ text "lt"
        text ", where the "
        el "code" $ text "+"
        text " cannot be reached. It turns out that the "
        el "code" $ text "+"
        text
            " isn't really necessary there, either. There are a couple of \
            \ambiguities, e.g. "
        el "em" $ text "holt"
        text " and "
        el "em" $ text "hold"
        text
            ", but they can be dealt with as exceptions. You will notice quickly \
            \that the "
        el "code" $ text "+"
        text
            " is omitted quite often—for a little bit increased typing efficiency."
        text "In the case of "
        el "em" $ text "scht"
        text ", omitting the "
        el "code" $ text "+"
        text " has the additional advantage that "
        el "code" $ text "-+ʃD"
        text " now exclusively means "
        el "em" $ text "cht"
        text ", which keeps things nice and simple."
    )

-- exercise6 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
-- exercise6 = exercise
--     6
--     (\_ -> el "p" $ do
--         text "For the "
--         el "code" $ text "F"
--         text " and the "
--         el "code" $ text "ʃ"
--         text
--             " key, the s in the coda is out of reach. Luckily we can use \
--                   \the (small) "
--         el "code" $ text "s"
--         text
--             " key in that case. Most of the examples here deal \
--                    \with \"fs\". The other common case is \"schs\". Depending on \
--                    \your keyboard, that combination might be difficult to reach. \
--                    \You can try and put your hands on a hand rest. Typically, \
--                    \the higher the hand rest, the easier it is to reach the keys."
--     )
--     PatSmallS
--     (\_ -> el "p" $ do
--         text
--             "You might notice that this exercise contains some words that \
--                   \are not terribly frequent. This just means that this basic use \
--                   \of the "
--         el "code" $ text "s"
--         text
--             "-key does not affect a lot of words. Later, we will learn \
--                    \other uses. This extra key will give us some flexibility \
--                    \when dealing with "
--         el "em" $ text "-st"
--         text ", "
--         el "em" $ text "-ds"
--         text ", and even "
--         el "em" $ text "-tzt"
--         text "."
--     )
--
--
--
-- exercise12 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
-- exercise12 = exercise
--     12
--     (\_ -> do
--         el "p" $ do
--             text
--                 "Now for something a bit different. \
--                       \A word part that ends with -dt, requires an additional stroke. \
--                       \The main reason for this is the fact that in German, \
--                       \there exists both, -dt and -tt next to regular -t. \
--                       \The idea behind this cumbersome extrastroke is, to have a \
--                       \distinct way of writing -dt, when the same word with -t exists, \
--                       \too. E.g. "
--             el "em" $ text "Brand"
--             text " and "
--             el "em" $ text "Brandt"
--             text "."
--         el "p" $ do
--             text
--                 "Luckily, there are not that many words at all that suffer \
--                       \from ambiguity regarding -dt and this exercise is quite small. \
--                       \A lot of -dt words are typed simply with "
--             el "code" $ text "-D"
--             text " alone and thus don't show up here."
--     )
--     PatDt
--     (\lang -> el "p" $ do
--         text
--             "You can find more information and examples of this \
--                   \rule in the correspondig section of the "
--         routeLink (stageUrl lang $ $readLoc "patternoverview")
--             $ text "pattern overview"
--         text "."
--     )
--
-- exercise13 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
-- exercise13 = exercise
--     13
--     (\_ -> el "p" $ do
--         text
--             "Here you find combinations of vowels in the nucleus that \
--                   \do not conform to the rules of simple, letter-by-letter \
--                   \replacement."
--     )
--     PatDiphtong
--     (\_ -> el "p" $ do
--         text
--             "If those seemed weird to you, that's because they are weird. \
--                   \Fortunately, the weird rules usually affect rare words."
--     )
--
-- exercise14 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
-- exercise14 = exercise
--     14
--     (\_ -> el "p" $ do
--         text "Often, when the letter "
--         el "em" $ text "c"
--         text " appears alone, it can be typed as "
--         el "code" $ text "G+-"
--         text " or "
--         el "code" $ text "-+G"
--         text
--             ", respectively. However, sometimes there exist two spellings \
--                    \alongside, one with "
--         el "em" $ text "c"
--         text " and one with "
--         el "em" $ text "k"
--         text
--             " and to allow you to distinctly type the c-variant, we need \
--                    \a custom pattern."
--     )
--     PatReplC
--     (\_ -> el "p" $ do
--         text "Also, not quite coincidentally, the code "
--         el "code" $ text "DʃG"
--         text
--             " that you learn here will appear again in the fingerspelling \
--                    \lesson."
--     )
--
-- exercise15 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
-- exercise15 = exercise
--     15
--     (\_ -> el "p" $ do
--         text
--             "This rule is here for compeleteness sake. Splitting up two \
--                   \letters into two chords is bad efficiency and will be dealt \
--                   \with by rules that follow in Stage 4. The list of words \
--                   \you have in this exercise is meant to shrink down and maybe \
--                   \can be optimized to nil."
--     )
--     PatBreakUpI
--     (\_ -> el "p" $ blank)
--
-- exercise16 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
-- exercise16 = exercise
--     16
--     (\_ -> el "p" $ do
--         text "There is one core lesson in this exercise: "
--         el "em" $ text "ts"
--         text " in the coda is typed using "
--         el "code" $ text " -+SD"
--         text ". An additional pattern is introduced to cover "
--         el "em" $ text "tst"
--         text " and "
--         el "em" $ text "sts"
--         text "."
--     )
--     PatSwapS
--     (\_ -> el "p" $ do
--         text
--             "This is the first of three exercises that deal with the \
--                    \sometimes convoluted consonant situation in the coda of German \
--                    \syllables. There are a lot of legitimate combinations of "
--         el "em" $ text "t"
--         text " with "
--         el "em" $ text "s"
--         text ", "
--         el "em" $ text "sch"
--         text "/"
--         el "em" $ text "ch"
--         text " and "
--         el "em" $ text "z"
--         text
--             ". You will notice similarities within these three exercises \
--                    \that hopefully help with learning."
--     )
--
-- exercise17 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
-- exercise17 = exercise
--     17
--     (\_ -> el "p" $ do
--         text "Similar to the last exercise, the core lesson here is: "
--         el "em" $ text "tsch"
--         text " is typed using "
--         el "code" $ text "+ʃD"
--         text "."
--     )
--     PatSwapSch
--     (\_ -> el "p" $ do
--         blank
--     )
--
-- exercise18 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
-- exercise18 = exercise
--     18
--     (\_ -> el "p" $ do
--         text "intro"
--     )
--     PatSwapZ
--     (\_ -> el "p" $ do
--         text "explication"
--     )
--
-- exercise19 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
-- exercise19 = exercise
--     19
--     (\_ -> el "p" $ do
--         text "intro"
--     )
--     PatDiVowel
--     (\_ -> el "p" $ do
--         text "explication"
--     )
--
-- exercise20 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
-- exercise20 = exercise
--     20
--     (\_ -> el "p" $ do
--         text "intro"
--     )
--     PatReplH
--     (\_ -> el "p" $ do
--         text "explication"
--     )
--
-- exercise21 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
-- exercise21 = mdo
--     Env {..} <- ask
--     let Navigation {..} = envNavigation
--     unless (navLang == DE) elNotImplemented
--
--     el "h1" $ text "Stage 3"
--     el "h2" $ text $ toDescription PatCodaGK
--     el "h3" $ text $ "Exercise 21"
--
--     el "p" $ text "intro"
--
--     elPatterns
--         $ Map.toList
--         $ Map.findWithDefault Map.empty 3
--         $ Map.findWithDefault Map.empty PatCodaGK patternDoc
--
--     el "p" $ text "explication"
--
--     evDone <- case getMapsForExercise PatCodaGK 3 of
--         Left str -> do
--             elClass "p" "small red" $ text $ "Couldn't load exercise: " <> str
--             pure never
--         Right (mSW, mWSs) -> taskWords
--             dynStatsAll
--             (gate (not <$> current dynDone) envEChord)
--             mSW
--             mWSs
--
--     dynStatsAll <- getStatsLocalAndRemote evDone
--
--     let dynStatsPersonal =
--             fmap snd . filter (isNothing . fst) . fmap snd <$> dynStatsAll
--     dynDone <- elCongraz (Just <$> evDone) dynStatsPersonal envNavigation
--     pure envNavigation
--
-- exercise22 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
-- exercise22 = exercise
--     22
--     (\_ -> el "p" $ do
--         text "intro"
--     )
--     PatReplRare
--     (\_ -> el "p" $ do
--         text "explication"
--     )
