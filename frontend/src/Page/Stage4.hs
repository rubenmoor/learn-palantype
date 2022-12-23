{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Page.Stage4 where

import Page.Common.Exercise (Constraints, exercise)
import           Palantype.DE                   ( Pattern(..) )
import           Reflex.Dom                     ( blank
                                                , current
                                                , gate
                                                , TriggerEvent
                                                , DomBuilder
                                                , EventWriter
                                                , MonadHold
                                                , PerformEvent
                                                , Performable
                                                , PostBuild
                                                , Prerender
                                                , el
                                                , elClass
                                                , never
                                                , text
                                                )
import           State                          ( Navigation(..)
                                                )

exercise1 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
exercise1 = exercise
    4 1
    (\_ -> do
        el "p" $ text
            "In the exercises of the last stage, you learned ground rules. \
            \They are ground rules, because they are marked \"Greediness 0\", \
            \G0 in short. The idea behind the G0 rules, is that any word \
            \that you ever need to write has at least one valid entry in \
            \the steno dictionary. And on top of this, alternative \
            \steno spellings are added."
        el "p" $ do
          text
            "The G0 codes are generally not the most efficient available steno \
            \code. An example: In the last stage you learned that "
          el "em" $ text "t"
          text " is typed using "
          el "code" $ text "D+"
          text " or "
          el "code" $ text "+D"
          text ", respectively. However, take the word "
          el "em" $ text "gut"
          text " and you will notice that there isn't really a good reason to \
               \bother with the "
          el "code" $ text "+"
          text " as the steno code "
          el "code" $ text "GUD"
          text " isn't occupied by any valid German word. This is one of the \
               \implicit advantage of any steno-style system: When typing in the \
               \conventional, serial style, every input is valid. \
               \When typing steno chords, \
               \we can recycle otherwise nonsensical inputs to increase \
               \efficiency."
        el "p" $ text
            "Any ruleset, like the one presented below, that has a Greediness \
            \bigger than 0 is a set of optional rules. All the words that you \
            \learn in this exercise already have a G0 code in the plover \
            \dictionary. Being greedy means, taking a shortcut: By cutting \
            \short the steno codes, the result is more efficient typing."
        el "p" $ do
          text "But we have to be careful with greediness. If you want to type "
          el "em" $ text "Rat"
          text " you still need the G0 code, as "
          el "code" $ text "RAD"
          text " results in "
          el "em" $ text "Rad"
          text ", which is a perfectly valid German word. In cases like this, \
               \the G0 rules take precedence. There are exceptions, but \
               \in practice you don't need to concern yourself with any of this. \
               \The words that you learn in the exercises here, always appear \
               \with their most efficient steno code possible. That means: the \
               \most greedy steno code that is not reserved already by another word \
               \that appears more frequently in the German language. The plover \
               \dictionary contains all valid steno codes, such that accidentally \
               \using a less-then-optimal steno code isn't a problem when using plover."
    )
    PatReplCommon1 2
    (\_ -> do
        el "p" $ text
            "Now there is a bit of a challenge regarding the optimal \
            \learning path. On the one hand, learning all the G0 rules \
            \first is a safe path to learning a valid steno code for any \
            \conceivable word, before you concern yourself with any \
            \optional rules for efficiency. On the other hand, \
            \it would be nice to learn the most common words first. \
            \This way, you may find yourself advancing much quicker."
        el "p" $ text
            "My proposed solution - if you follow the exercises in order - \
            \is to interweave the greedier rules in between the G0 rules. \
            \There more words are covered by a rule, regardless of its greediness \
            \the earlier it appears in the exercises."
    )

exercise2 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
exercise2 = exercise
    4 3
    (\_ ->
        el "p" $ text
            "Jumping back to \"Replacements of Common Words 1\" once more, \
            \with increased greediness. By chosing a higher value for the \
            \greediness for this particular rule, we make sure that the \
            \algorithm that generates the steno dictionary applies this \
            \rule as an independent optimization."
    )
    PatReplCommon1 3
    (\_ -> do
        el "p" $ do
            text
                "Because the greedy optimizations fail every time the resulting \
                \code is already in use by some other, more frequent word."
    )

exercise3 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
exercise3 = exercise
    4 4
    (\_ ->
        el "p" do
            text "Probably the most important rule here is the last one for "
            el "em" $ text "y"
            text "."
    )
    PatReplCommon2 4
    (\_ -> do
        el "p" $ do
            text "And you will find a lot of anglicisms in this exercise. \
                 \Those are generally quite a challenge for a German steno \
                 \system and are the cause for inconsistencies and quirks \
                 \in the rules. There are steno rules that try to tackle \
                 \anglicisms in particular in later stages."
    )
