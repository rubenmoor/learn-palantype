{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Page.Stage5 where

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
import Palantype.Common (Greediness, Lang (..), Palantype, toDescription)
import Palantype.Common.TH (failure, readLoc)
import Palantype.DE (Pattern (..))
import Reflex.Dom (blank, current, gate, TriggerEvent, DomBuilder, EventWriter, MonadHold, PerformEvent, Performable, PostBuild, Prerender, delay, el, elClass, getPostBuild, never, switchDyn, text, widgetHold, widgetHold_)
import State (Env (..), Navigation (..), State, stageUrl)
import Text.Read (readMaybe)
import TextShow (TextShow (showt))
import Data.Maybe (isNothing)

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
  -> Greediness
  -> (Lang -> m ())
  -> m Navigation
exercise iEx elIntro pat greediness elExplication = mdo
    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang == DE) elNotImplemented

    el "h1" $ text "Stage 5"
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

    dynStatsAll <- getStatsLocalAndRemote evDone
    evEDict <- request $ getDictDE' pat greediness ePb
    evDone <- fmap switchDyn $ widgetHold (loading $> never) $
        evEDict <&> \case
            Right (mSW, mWSs) -> if null mSW
              then do el "p" $
                        text "There are no words in this exercise. \
                          \This is probably an error. \
                          \Skip this for now."
                      pure never
              else taskWords dynStatsAll (gate (not <$> current dynDone) envEChord) mSW mWSs
            Left str -> never <$ elClass
                        "div"
                        "paragraph small red"
                        (text $ "Could not load resource: dict: " <> str)

    let dynStatsPersonal = fmap snd . filter (isNothing . fst) . fmap snd <$> dynStatsAll
    dynDone <- elCongraz (Just <$> evDone) dynStatsPersonal envNavigation
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
        )
        PatBrief
        0
        ( \navLang -> do
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
        )
