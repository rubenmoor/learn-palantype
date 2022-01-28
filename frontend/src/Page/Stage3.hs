module Page.Stage3 where

import Servant.Common.Req (reqSuccess, ReqResult(ResponseSuccess))
import Reflex.Dom (switchDyn, never, widgetHold, blank, widgetHold_, getPostBuild, delay, elClass, text, el, Prerender, PostBuild, MonadHold, EventWriter, DomBuilder)
import Data.Semigroup (Endo)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader.Class (ask, MonadReader)
import Palantype.Common (toDescription, Lang (..), Palantype)
import Obelisk.Route.Frontend (SetRoute)
import Common.Route (FrontendRoute)
import Obelisk.Route (R)
import State (Navigation (..), Env (..), State)
import Control.Monad (unless)
import Palantype.DE (Pattern(..))
import Page.Common (elPatterns, elCongraz, taskWords, loading, elNotImplemented)
import Client (request, getDocDEPattern', postRender, getDictDE')
import Data.Witherable (Filterable(mapMaybe))
import Data.Functor (($>), (<&>))
import Data.Foldable (traverse_)
import Data.Text (Text)
import TextShow (TextShow(showt))

exercise
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       )
    => Int
    -> m ()
    -> Pattern
    -> m ()
    -> m Navigation
exercise iEx elIntro pat elExplication = do

    Env {..} <- ask
    let Navigation {..} = envNavigation
    unless (navLang == DE) elNotImplemented

    el "h1" $ text "Stage 3"
    el "h2" $ text $ toDescription pat
    el "h3" $ text $ "Exercise " <> showt iEx

    elIntro

    ePb    <- postRender $ delay 0.1 =<< getPostBuild
    evEDoc <- request $ getDocDEPattern' pat 0 ePb
    widgetHold_ loading $ evEDoc <&> \case
      Right doc -> elPatterns doc
      Left  str -> elClass "div" "paragraph small red"
        $ text $ "Could not load resource: docs: " <> str

    elExplication

    elClass "div" "paragraph" $ text "Type the following words as they appear!"

    evEDict <- request $ getDictDE' pat 0 ePb
    eDone <- fmap switchDyn $ widgetHold (loading $> never) $ evEDict <&> \case
      Right (mSW, mWSs) -> taskWords mSW mWSs
      Left  str         -> never <$ elClass "div" "paragraph small red"
        ( text $ "Could not load resource: dict: " <> str)

    elCongraz eDone envNavigation
    pure envNavigation

exercise1
    :: forall key t (m :: * -> *) . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise1 =
  exercise
    1
    (elClass "div" "paragraph" $ text
       "In general, any word in the natural language translates to some \
       \steno code based on a couple of straightforward substitutions. \
       \In this exercise, we start with the most common ones."
    )
    PatReplCommon
    (elClass "div" "paragraph" $ text
       "First of all, note that these patterns are in addition to the \
       \simple patterns of the last exercise. \
       \Thus, if you are missing a letter, it might be among the \
       \simple patterns. Also, note that the minus sign, -, isn't an actual \
       \steno key. Instead, it's used to distinguish between the left-hand and \
       \right-hand version of keys that appear twice on the steno keyboard."
    )

exercise2
    :: forall key t (m :: * -> *) . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise2 =
  exercise
    2
    (elClass "div" "paragraph" $ text
       "Now you'll learn a nice simplification. When you encounter one of the \
       \double consonants of the table below in the coda, you will only need one \
       \steno key to type them. \
       \The ss is an exception and the reason is simple: unlike the other \
       \consonants, the s has two keys for your right hand."
    )
    PatDiConsonant
    (elClass "div" "paragraph" $ text
       "This rule only is about double consonants in the coda. \
       \Quite often, a double consonant is devided by an ortographic syllable \
       \and this rule doesn't apply. In case you wonder what german syllable \
       \ever ends on dd: None of them. This entry is there only for anglicisms."
    )

exercise3
    :: forall key t (m :: * -> *) . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise3 =
  exercise
    3
    (elClass "div" "paragraph" $ do
        text
          "You might have wondered why there is no key for h for your right hand. \
          \Vowels that are being stretched out, e.g. with an h, are typed using \
          \one of the stretch keys. You can call "
        el "code" $ text "~"
        el "em" $ text
          "stretch"
        text " or "
        el "em" $ text
          "Dehnung"
        text ". With "
        el "code" $ text "~"
        text " you can stretch any of the vowels of your right hand. In order to \
             \stretch the vowels of your right hand, you have "
        el "code" $ text "Ü"
        text ", which doubles as a second stretch key."
    )
    PatCodaH
    (elClass "div" "paragraph" $ do
        text
          "Note that the stretch key isn't only for h, but it also turns i into ie. \
          \Another thing: "
        el "em" $ text "ö"
        text " is typed using two keys, already, and its stretched version "
        el "em" $ text "öh"
        text " isn't typed any differently."
    )

exercise4
    :: forall key t (m :: * -> *) . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise4 =
  exercise
    4
    (elClass "div" "paragraph" $ do
       text "This might come as a little surprise but, just like with "
       el "em" $ text "h"
       text ", the letter "
       el "em" $ text "r"
       text " doesn't have a key for your right hand either and it is \
            \typed using the same stretch keys."
    )
    PatCodaR
    (elClass "div" "paragraph" $ do
        text "There is the exception of "
        el "em" $ text "ur"
        text ", which uses "
        el "code" $ text "+"
        text " to leave "
        el "code" $ text "~U"
        text " for the way more frequent "
        el "em" $ text "uhr"
        text ". But about this one, we will learn in the next excercise."
    )

exercise5
    :: forall key t (m :: * -> *) . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise5 =
  exercise
    5
    (elClass "div" "paragraph" $ do
        text "intro"
    )
    PatCodaRR
    (elClass "div" "paragraph" $ do
        text "explication"
    )

exercise6
    :: forall key t (m :: * -> *) . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise6 =
  exercise
    5
    (elClass "div" "paragraph" $ do
        text "intro"
    )
    PatCodaRR
    (elClass "div" "paragraph" $ do
        text "explication"
    )

exercise7
    :: forall key t (m :: * -> *) . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise7 =
  exercise
    5
    (elClass "div" "paragraph" $ do
        text "intro"
    )
    PatCodaRR
    (elClass "div" "paragraph" $ do
        text "explication"
    )

exercise8
  :: forall key t (m :: * -> *) . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise8 =
  exercise
    5
    (elClass "div" "paragraph" $ do
        text "intro"
    )
    PatCodaRR
    (elClass "div" "paragraph" $ do
        text "explication"
    )

exercise9
    :: forall key t (m :: * -> *) . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise9 =
  exercise
    5
    (elClass "div" "paragraph" $ do
        text "intro"
    )
    PatCodaRR
    (elClass "div" "paragraph" $ do
        text "explication"
    )

exercise10
    :: forall key t (m :: * -> *) . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise10 =
  exercise
    5
    (elClass "div" "paragraph" $ do
        text "intro"
    )
    PatCodaRR
    (elClass "div" "paragraph" $ do
        text "explication"
    )

exercise11
    :: forall key t (m :: * -> *) . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise11 =
  exercise
    5
    (elClass "div" "paragraph" $ do
        text "intro"
    )
    PatCodaRR
    (elClass "div" "paragraph" $ do
        text "explication"
    )

exercise12
    :: forall key t (m :: * -> *) . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise12 =
  exercise
    5
    (elClass "div" "paragraph" $ do
        text "intro"
    )
    PatCodaRR
    (elClass "div" "paragraph" $ do
        text "explication"
    )

exercise13
    :: forall key t (m :: * -> *) . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise13 =
  exercise
    5
    (elClass "div" "paragraph" $ do
        text "intro"
    )
    PatCodaRR
    (elClass "div" "paragraph" $ do
        text "explication"
    )

exercise14
    :: forall key t (m :: * -> *) . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise14 =
  exercise
    5
    (elClass "div" "paragraph" $ do
        text "intro"
    )
    PatCodaRR
    (elClass "div" "paragraph" $ do
        text "explication"
    )

exercise15
    :: forall key t (m :: * -> *) . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise15 =
  exercise
    5
    (elClass "div" "paragraph" $ do
        text "intro"
    )
    PatCodaRR
    (elClass "div" "paragraph" $ do
        text "explication"
    )

exercise16
    :: forall key t (m :: * -> *) . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Env t key) m
       , Palantype key
       , PostBuild t m
       , Prerender t m
       , SetRoute t (R FrontendRoute) m
       )
    => m Navigation
exercise16 =
  exercise
    5
    (elClass "div" "paragraph" $ do
        text "intro"
    )
    PatCodaRR
    (elClass "div" "paragraph" $ do
        text "explication"
    )

{-
  | PatReplCommon  DONE 1
  | PatDiConsonant DONE 2
  | PatCodaH       DONE 3
  | PatCodaR       DONE 4
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
