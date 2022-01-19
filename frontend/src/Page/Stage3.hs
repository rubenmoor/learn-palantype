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
import Page.Common (elCongraz, taskWords, loading, elNotImplemented)
import Client (postRender, getDictDE')
import Data.Witherable (Filterable(mapMaybe))
import Data.Functor ((<&>))

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
              "Introducing words that rely on two or more chords to type now. \
              \In general, the idea of any steno system is typing efficiency \
              \and the less chords you need to type a word, the better. \
              \There are, however, several reasons why especially German words \
              \do not always fit into one steno chord. In that case, \
              \the words are simply split up and you type the corresponding chord \
              \in succession to produce the word."

    elClass "div" "paragraph" $ text "Type the following words as they appear!"

    ePb     <- postRender $ delay 0.1 =<< getPostBuild
    eResult <- postRender $ getDictDE' PatReplCommon 0 ePb
    let
        eSuccess = mapMaybe reqSuccess eResult

    widgetHold_ loading $ eResult <&> \case
      ResponseSuccess {} -> blank
      _                  -> elClass "div" "paragraph small red"
        $ text "Could not load resource: dict"

    eDone <- taskWords eSuccess

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
