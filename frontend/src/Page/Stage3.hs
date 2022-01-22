module Page.Stage3 where

import Servant.Common.Req (reqSuccess, ReqResult(ResponseSuccess))
import Reflex.Dom (blank, widgetHold_, getPostBuild, delay, elClass, text, el, Prerender, PostBuild, MonadHold, EventWriter, DomBuilder)
import Data.Semigroup (Endo)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader.Class (ask, MonadReader)
import Palantype.Common (Lang (..), Palantype)
import Obelisk.Route.Frontend (SetRoute, RouteToUrl)
import Common.Route (FrontendRoute)
import Obelisk.Route (R)
import State (Navigation (..), Env (..), State)
import Control.Monad (unless)
import Palantype.DE (Pattern(PatReplCommon))
import Page.Common (elPatterns, elCongraz, taskWords, loading, elNotImplemented)
import Client (getDocDEPattern', postRender, getDictDE')
import Data.Witherable (Filterable(mapMaybe))
import Data.Functor ((<&>))
import Data.Foldable (traverse_)

exercise1
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise1 = do

    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang == DE) elNotImplemented

    el "h1" $ text "Stage 3"
    el "h2" $ text "Common replacement rules"
    el "h3" $ text "Exercise 1"

    elClass "div" "paragraph"
        $ text
              "In general, any word in the natural language translates to some \
              \steno code based on a couple of straightforward substitutions. \
              \We start with the most common ones now."

    elClass "div" "paragraph"
        $ text
              "First of all, note that these patterns are in addition to the \
              \simple patterns of the last exercise."

    ePb      <- postRender $ delay 0.1 =<< getPostBuild
    eResDict <- postRender $ getDictDE' PatReplCommon 0 ePb
    let
        eDict = mapMaybe reqSuccess eResDict

    eResDoc <- postRender $ getDocDEPattern' PatReplCommon 0 ePb

    widgetHold_ loading $ eResDict <&> \case
      ResponseSuccess {} -> blank
      _                  -> elClass "div" "paragraph small red"
        $ text "Could not load resource: dict"

    widgetHold_ loading $ eResDoc <&> \case
      ResponseSuccess {} -> blank
      _                  -> elClass "div" "paragraph small red"
        $ text "Could not load resource: docs"

    let eDoc = mapMaybe reqSuccess eResDoc
    widgetHold_ blank $ eDoc <&> \ls ->
      elClass "div" "patternTable" $ traverse_ elPatterns ls

    eDone <- taskWords eDict

    elClass "div" "paragraph"
        $ text
              "Like with a lot of rules, there are exceptions. \
           \We don't need to bother right now, the words of this exercise \
           \are not affected. Just that you now, \
           \sometimes you will have to type chords that span multiple \
           \syllables and sometimes you will need multiple chords to \
           \type a single syllable. For this reason we generally speak of \
           \word parts instead of syllables."

    elCongraz eDone envNavigation
    pure envNavigation

{-
  | PatReplCommon
  | PatDiConsonant
  | PatCodaH
  | PatCodaR
  | PatCodaRR
  | PatCodaHR
  | PatDt
  | PatDiphtong
  | PatReplC
  | PatCodaGK
  | PatSZ
  | PatIJ
  | PatTsDsPs
  | PatDiVowel
  | PatReplH
  | PatSmallS
-}
