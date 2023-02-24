{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}

module Page.Stage14 where

import           Common.Route                   ( FrontendRoute )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader.Class     ( MonadReader )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Semigroup                 ( Endo )
import           Obelisk.Route                  ( R )
import           Obelisk.Route.Frontend         ( RouteToUrl
                                                , Routed
                                                , SetRoute
                                                )
import           Palantype.Common               ( Greediness
                                                , Palantype
                                                , StageIndex
                                                )
import           Palantype.DE                   ( Pattern(..) )
import qualified Palantype.DE                  as DE
import           Reflex.Dom                     ( DomBuilder
                                                , EventWriter
                                                , MonadHold
                                                , PerformEvent
                                                , Performable
                                                , PostBuild
                                                , Prerender
                                                , TriggerEvent
                                                , blank
                                                , el
                                                , elClass
                                                , text
                                                )
import           State                          ( Env(..)
                                                , State
                                                )

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
    , Routed t StageIndex m
    , RouteToUrl (R FrontendRoute) m
    , SetRoute t (R FrontendRoute) m
    , TriggerEvent t m
    )

exercises
  :: forall key t (m :: * -> *)
  . Constraints key t m
  => Map (DE.Pattern, Greediness) (m (), m ())
exercises = Map.fromList
  [ ( ( PatBrief,  0)
    , ( do
          el "p" $
              text
                  "One major reason why steno typing allows for such a high \
                  \speed is the existence of briefs. A brief is a steno code \
                  \that is used for one specific word or a syllable. \
                  \Technically, every brief is an additional rule. Less \
                  \technical, every brief is an exception to the rules that \
                  \you learned so far. The steno code for briefs is meant to \
                  \make very common words much quicker to type. And don't be \
                  \scared: you will see that most briefs aren't that hard to \
                  \memorize. While briefs ignore all the rules, usually they \
                  \can be designed in ways that kind of make sense."

          el "p" do
              text "Experienced stenotypists design briefs all the time, \
                   \sometimes along the way as they type. Imagine you are \
                   \transcribing an interview with the "
              el "em" $ text "Bundesgesundheitsministerin"
              text " and she keeps talking about the "
              el "em" $ text "Hackfleischverordnung"
              text ". Having those two words encoded as one or two chords \
                   \, opposed to ten and five chords, respectively \
                   \not only makes you quicker. Long words can be quite \
                   \demanding to type, in general."
          el "p" do
              text "Note, however, that the briefs you are learning here \
                   \are less specific. Rather than cutting down very long \
                   \words, you find the most common words of the German \
                   \language here. Often the briefs just turn a two-chord \
                   \code into a single chord. Sometimes the briefs aren't \
                   \increasing efficiency but rather circumvent collisions."

          el "p" do
              text "While the above list contains all the information \
                   \necessary to complete this exercise, it doesn't help a lot with \
                   \learning. Within the briefs there are repeating patterns. \
                   \The following codes are built around the idea of shortening a \
                   \two-syllable word to one syllable by cutting off stuff at the \
                   \end (try to guess the real-life word just by reading the code): "


          el "p" $
              el "code" $ text "DAB DAF DAʃG OD ~ON U+ND F+~UD F+~ÜD"

          el "p" do
              text "The codes for "
              el "em" $ text "werden"
              text ", "
              el "em" $ text "worden"
              text ", "
              el "em" $ text "wurden"
              text ", and "
              el "em" $ text "würden"
              text " do not appear hear, as there is a regular rule already \
                    \that allows to replace "
              el "em" $ text "den"
              text " with "
              el "code" $ text "-n"
              text ". Sometimes cutting stuff at the end isn't a good \
                   \strategy, because it may result in more ambiguous steno. \
                   \Of course, we can cut stuff off the front, too. \
                   \Try to guess those ones:"

          el "p" $
              el "code" $ text "DF+A DF+AS BEÜ"

          el "p" $ text "For good measure, we can also cut stuff in the middle: "

          el "p" $
              el "code" $ text "FNEO+ʃ F+N"

          el "p" $ text "... or a bit everywhere: "
          el "p" $
              el "code" $ text "BOGʃ"

          el "p" do
              text "The majority of the steno code in this exercise \
                   \follows a pattern that incorporates inflection. \
                   \E.g. you can think of "
              el "code" $ text "LAL"
              text " as the core steno code for "
              el "em" $ text "all-"
              text ", and then complete the different forms according \
                   \to the table:"

          elClass "div" "patternTable" do
            elClass "div" "floatLeft" do
              elClass "div" "orig" $ text "alle"
              elClass "code" "steno" $ text "LALn"

            elClass "div" "floatLeft" do
              elClass "div" "orig" $ text "aller"
              elClass "code" "steno" $ text "LALs"

            elClass "div" "floatLeft" do
              elClass "div" "orig" $ text "alles"
              elClass "code" "steno" $ text "LALS"

            elClass "div" "floatLeft" do
              elClass "div" "orig" $ text "allem"
              elClass "code" "steno" $ text "LALM"

            elClass "div" "floatLeft" do
              elClass "div" "orig" $ text "allen"
              elClass "code" "steno" $ text "LALN"

          elClass "br" "clearBoth" blank

          el "p" do
            text "To come up with a name for this rule, read the endings \
                 \backwards and you have the Enemerese-rule. Apart from "
            el "em" $ text "alle"
            text " it applies to:"

          el "dl" do
              el "dt" do
                  el "em" $ text "dies-"
                  text " "
                  el "code" $ text "D~I"
              el "dd" do
                  text "Exception: "
                  el "em" $ text "dieses"
                  text " uses "
                  el "code" $ text "D~ISs"
                  text " as "
                  el "code" $ text "D~IS"
                  text " is in use for "
                  el "em" $ text "dies"
                  text " already."

              el "dt" do
                  el "em" $ text "ein-"
                  text " "
                  el "code" $ text "EI"
              el "dd" do
                  text "Exception: "
                  el "em" $ text "einen"
                  text " uses the regular en-rule and thus "
                  el "code" $ text "EINn"
                  text " as "
                  el "code" $ text "EIN"
                  text " is in use for "
                  el "em" $ text "ein"
                  text " already, and "
                  el "em" $ text "eines"
                  text " uses "
                  el "code" $ text "EINs"
                  text " as "
                  el "code" $ text "EIS"
                  text " is in use for, well, "
                  el "em" $ text "Eis"
                  text "."

              el "dt" do
                  el "em" $ text "ihr-"
                  text " "
                  el "code" $ text "~I"
              el "dd" do
                  text "Exception: "
                  el "em" $ text "ihren"
                  text " uses "
                  el "code" $ text "~I+N"
                  text " as "
                  el "code" $ text "~IN"
                  text " is in use for "
                  el "em" $ text "ihn"
                  text " already."

              el "dt" do
                  el "em" $ text "kein-"
                  text " "
                  el "code" $ text "G+EI"
              el "dd" do
                  text "No Exception."

              el "dt" do
                  el "em" $ text "neu-"
                  text " "
                  el "code" $ text "NE"
              el "dd" $ text "No Exception."

              el "dt" do
                  el "em" $ text "sein-"
                  text " "
                  el "code" $ text "SEI"
              el "dd" $ text "No Exception."

              el "dt" do
                  el "em" $ text "viel-"
                  text " "
                  el "code" $ text "FBLI"
              el "dd" $ text "No Exception."

          el "p" $ text "While we covered the majority of the available \
                        \briefs with explanations of their origin, there \
                        \remains a number of them being quite irregular. \
                        \In order to practice now, you might find it useful \
                        \have the complete list, right here."
      , blank
      )
    )
  ]
