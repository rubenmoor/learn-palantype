{-# LANGUAGE TypeApplications #-}
module Page.Stage11 where

import           Page.Common.Exercise           ( Constraints )
import           Palantype.Common               ( Greediness )
import           Palantype.DE                   ( Pattern(..) )
import           Reflex.Dom                     ( el
                                                , text
                                                )
import           Data.Map.Strict                ( Map )
import qualified Palantype.DE                  as DE
import qualified Data.Map.Strict               as Map

import State (stageUrl)
import Obelisk.Route.Frontend (routeLink)

exercises
    :: forall key t (m :: * -> *)
     . Constraints key t m
    => Map (DE.Pattern, Greediness) (m (), m ())
exercises = Map.fromList
    [ ( (PatDiVowel, 0)
      , ( el "p" $ text "TODO"
        , el "p" $ text "TODO"
        )
    )
    , ( (PatDiVowel, 1)
      , ( el "p" $ text "TODO"
        , el "p" $ text "TODO"
        )
      )
    , ( (PatDiVowel, 4)
      , ( el "p" $ text "TODO"
        , el "p" $ text "TODO"
        )
      )
    , ( (PatCodaGK, 3)
      , ( do
            el "p" $ do
              text "The word "
              el "em" $ text "abgehakt"
              text " has a long "
              el "em" $ text "a"
              text ". Just compare to "
              el "em" $ text "abgehackt"
              text ". Unfortunately, the is no way to tell this kind of long "
              el "em" $ text "a"
              text " from a short "
              el "em" $ text "a"
              text " based on orthography alone. Compare with the words "
              el "em" $ text "praktisch"
              text ", "
              el "em" $ text "Fakt"
              text ", or "
              el "em" $ text "Pakt"
              text ". A solution based on orthography would be: a special steno \
                   \code for "
              el "em" $ text "ck"
              text " and a greedier rule that allows to treat "
              el "em" $ text "ck"
              text " just like "
              el "em" $ text "k"
              text " as long as the steno code for the whole world is \
                   \unambiguous. You see that we take another route. In "
              routeLink (stageUrl @key 13) $ text "Exercise 3.1"
              text " you learned that "
              el "em" $ text "k"
              text " and "
              el "em" $ text "ck"
              text " are treated with the same steno. The reason is that the \
                   \need to differentiate between the two is so rare that we \
                   \treat it as exception."
            el "p" $ do
              text "In this exercise you find words with a long vowel before \
                   \the "
              el "em" $ text "k"
              text ". And this long vowel is stretched using the stretch key."
        , el "p" $ do
            text "The idea behind this rule: You can still treat "
            el "em" $ text "k"
            text " and "
            el "em" $ text "ck"
            text " the same and will only encounter problems when you hit upon \
                 \one of the above words."
        )
      )
    ]
