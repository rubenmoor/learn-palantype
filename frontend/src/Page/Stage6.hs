{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Page.Stage6 where

import           Obelisk.Route.Frontend         ( routeLink
                                                )
import Page.Common.Exercise (Constraints)
import           Palantype.Common               (mapStages, Greediness, StageSpecialGeneric(..), findStage)
import           Palantype.DE                   ( Pattern(..) )
import           Reflex.Dom                     ( blank
                                                , el, text)
import           State                          ( stageUrl
                                                )
import           TextShow                       ( TextShow(showt) )
import Data.Map.Strict (Map)
import qualified Palantype.DE as DE
import qualified Data.Map.Strict as Map
import Palantype.Common.TH (fromJust)

exercises
  :: forall key t (m :: * -> *)
  . Constraints key t m
  => Map (DE.Pattern, Greediness) (m (), m ())
exercises = Map.fromList
  [ ( (PatCodaH, 0)
    , ( el "p" $ do
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
                    \stretch the vowels of your left hand, you have "
          el "code" $ text "Ü"
          text ", which doubles as a second stretch key."
      , do
          el "p" $ do
            text
                "Note that the stretch key isn't only for h, but it also turns "
            el "em" $ text "i"
            text "into "
            el "em" $ text "ie"
            text ". There is something to say about "
            el "em" $ text "ie"
            text " in words like "
            el "em" $ text "Linie"
            text ", "
            el "em" $ text "Serie"
            text " oder "
            el "em" $ text "Aktie"
            text ". In these words, technically, the letters "
            el "em" $ text "i"
            text " and "
            el "em" $ text "e"
            text
                " do not have anything to do with vowel stretching. \
                   \In order to learn how to type fast, this is of no concern. \
                   \Just treat all cases of "
            el "em" $ text "ie"
            text " the same."

          el "p" $ do
            text "Another thing: "
            el "em" $ text "ö"
            text " is typed using two keys, already, and its stretched version "
            el "em" $ text "öh"
            text " isn't typed any differently."
      )
    )
  , ( (PatCodaH, 1)
    , ( el "p" $
          text "The last rule was a G0 rule, now comes the more efficient version. \
            \Only about 1% of the words that have a long vowel in terms of the \
            \rule, cannot be typed more efficiently according to this greedier \
            \rule."
      , blank
      )
    )
  , ( (PatCodaR, 0)
    , ( el "p" $ do
          text "This might come as a little surprise but, just like with "
          el "em" $ text "h"
          text ", the letter "
          el "em" $ text "r"
          text " doesn't have a key for your right hand either and it is \
               \typed using the same stretch keys."
      , do
          el "p" $ do
              text "As in the previous exercise, "
              el "em" $ text "ö"
              text
                  " already uses two keys and its stretched version doesn't \
                    \look any different then the unstretched "
              el "em" $ text "ö"
              text "."
          el "p" $ do
              text
                  "In case you wonder: This is the reason why the stretch key \
                     \is not simply called H. Using one and the same key for \
                     \several letters causes conflicts. Think of the words "
              el "em" $ text "eh"
              text " and "
              el "em" $ text "er"
              text ", "
              el "em" $ text "Horn"
              text " and "
              el "em" $ text "Hohn"
              text ", "
              el "em" $ text "Sie"
              text " and "
              el "em" $ text "Sir"
              text
                  " … So, why going through all these troubles? Well, in German, \
                     \the coda tends to get quite juicy, combining a lot of \
                     \consonants, e.g. "
              el "em" $ text "plantschst"
              text ", "
              el "em" $ text "seufzt"
              text ", or "
              el "em" $ text "knirschst"
              text
                  ". These latter words aren't specialties but rather regular \
                     \features of the language—brought about by conjugation. \
                     \In order to accommodate these, we need to shift some weight \
                     \away from the four fingers of the right hand. The thumbs, \
                     \which take care of vowels, can take over the responsibility for "
              el "em" $ text "h"
              text " and "
              el "em" $ text "r"
              text
                  " just fine. And even though we don't adhere to \
                     \the rules of linguistics in any strict sense, \
                     \it is a welcome fact that, in German, "
              el "em" $ text "r"
              text " preceded by a vowel is not pronounced anything like "
              el "em" $ text "r"
              text
                  " in the onset of a syllable. So all in all it is quite convenient \
                     \to treat "
              el "em" $ text "r"
              text " in the coda as sort of a modifier to a vowel."
      )
    )
  , ( (PatCodaR, 4)
    , ( el "p" $ text "This rule needs to show up somewhere for completeness."
      , blank
      )
    )
  , ( (PatCodaRR, 0)
    , ( el "p" $ do
          text "Following the idea of stretching vowels, a "
          el "em" $ text "rr"
          text
              " is yet another way to stretch a vowel and it's typed \
                    \using the "
          el "code" $ text "+"
          text " key."
      , el "p" $ do
          text "Do not let the appereance of "
          el "em" $ text "t"
          text
              " in the patterns disturb you. There is a simple reason: \
                    \You have learned to type "
          el "em" $ text "t"
          text " using "
          el "code" $ text "+D"
          text " in "
          let (i, t, s) = $fromJust $ findStage mapStages $ StageGeneric DE.PatReplCommon1 0
          routeLink (stageUrl @key i)
              $  text
              $  "Exercise "
              <> showt t
              <> "."
              <> showt s
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
    )
  , ( (PatCodaHR, 0)
    , (el "p" $ text
                  "This is the last rule regarding regular vowel streting and \
                  \it is fairly straightforward."
      , el "p" $ do
          text "Use "
          el "code" $ text "~"
          text " with "
          el "code" $ text "+"
          text " to reach "
          el "em" $ text "hr"
          text " in the coda."
      )
    )
  ]
