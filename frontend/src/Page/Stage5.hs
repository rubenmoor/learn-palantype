{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Page.Stage5 where

import Page.Common.Exercise (Constraints, exercise)
import           Palantype.DE                   ( Pattern(..) )
import           Reflex.Dom                     ( blank
                                                , el
                                                , text
                                                )
import           State                          ( Navigation(..)
                                                )

exercise1 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
exercise1 = exercise
    5 1
    (el "p" $ do
        el "em" $ text "R"
        text
            " is quite a common letter in German, still it is missing from the \
            \keyboard. To see why, note that in German a syllable combines a lot \
            \of consonants. In the onset, there are "
        el "em" $ text "dr"
        text ", "
        el "em" $ text "tr"
        text ", "
        el "em" $ text "schr"
        text ", "
        el "em" $ text "fr"
        text ", "
        el "em" $ text "gr"
        text ", "
        el "em" $ text "kr"
        text ", "
        el "em" $ text "br"
        text ", and "
        el "em" $ text "pr"
        text ", and even "
        el "em" $ text "str"
        text ", "
        el "em" $ text "spr"
        text ". The straightforward way to implement "
        el "em" $ text "R"
        text
            " thus would be an R-key on the index finger of the left hand, to \
            \the right of all the other keys, including "
        el "code" $ text "+"
        text
            ". You see, we are simply running out of space and need a different \
            \solution. The basic idea is that the "
        el "code" $ text "M"
        text "-key and the "
        el "code" $ text "L"
        text "-key fill the role of "
        el "em" $ text "r"
        text "in the onset. They can't combine with "
        el "em" $ text "r"
        text " themselves, which is fine. In particular, you use "
        el "code" $ text "M"
        text " in the most simple case and "
        el "code" $ text "L"
        text " to type "
        el "em" $ text "tr"
        text " and "
        el "em" $ text "spr"
        text ". Unfortunately, this is not enough and we need to add "
        el "code" $ text "D"
        text " as a replacement for "
        el "code" $ text "+"
        text " in a couple of cases. "
    )
    PatOnsetR 0
    (el "p" $ do
        text "And don't forget: The "
        el "em" $ text "r"
        text " without any other consonants exists, too."
    )

exercise2 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
exercise2 = exercise
    5 2
    (el "p" $ do
        text "The keys "
        el "code" $ text "+"
        text " and "
        el "code" $ text "L"
        text " sharing the same finger, implies that we need to use "
        el "code" $ text "D"
        text " as a replacement for "
        el "code" $ text "+"
        text " sometimes. Note that we did the same in the last exercise."
    )
    PatOnsetL 0
    blank


exercise3 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
exercise3 = exercise
    5 3
    ( el "p" $ text
          "Now you'll learn a nice simplification. When you encounter one of the \
          \double consonants of the table below in the coda, you will only need one \
          \steno key to type them."
    )
    PatDiConsonant 0
    ( el "p" $ text
            "This rule only is about double consonants in the coda. \
            \Quite often, a double consonant is devided by an ortographic syllable \
            \and this rule doesn't apply. In case you wonder what german syllable \
            \ever ends on dd: None of them. This entry is there only for anglicisms."
    )

exercise4 :: forall key t (m :: * -> *) . Constraints key t m => m Navigation
exercise4 = exercise
    5 4
    ( el "p" do
            text "In this stage, this is the only exercise that has a greediness \
                 \bigger then 0 and thus introduces additional efficiency. \
                 \No surprises here: When "
            el "em" $ text "t"
            text " can be greedily reached by "
            el "code" $ text "D"
            text " and doubled consonants can be greedily reached as if they \
                 \were singletons, then we can add some greed and combine \
                 \the idea of the various rules."
    )
    PatDiConsonant 2
    (el "p" $ text
         "And you will find a lot of anglicisms in this exercise. \
         \Those are generally quite a challenge for a German steno \
         \system and are the cause for inconsistencies and quirks \
         \in the rules. There are steno rules that try to tackle \
         \anglicisms in particular in later stages."
    )
