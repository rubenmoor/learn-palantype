{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BlockArguments #-}

module AdminPages where

import Data.Generics.Sum (_As)
import Data.Generics.Product (field)
import Control.Lens.Getter (view, (^.))
import Control.Lens.Setter ((.~))
import           Reflex.Dom                     (dynText, tag, EventName (Click), domEvent, current, gate, leftmost, prerender, updated, inputElementConfig_setValue, DomBuilderSpace, InputElementConfig, inputElementConfig_initialChecked, elementConfig_initialAttributes, inputElementConfig_elementConfig, inputElement, InputElement (..)
                                                , EventWriter
                                                , constDyn
                                                , (=:)
                                                , elAttr
                                                , DomBuilder
                                                , MonadHold
                                                , elClass
                                                , widgetHold_
                                                , text
                                                , el
                                                , Prerender
                                                , Dynamic
                                                , PostBuild

                                                , blank
                                                )
import           Control.Monad.Reader           ( MonadReader(ask) )
import           Data.Foldable                  ( for_ )
import           Data.Functor                   ((<$>)
                                                , (<&>)
                                                )
import           Data.Either                    (either,  Either(..) )
import           Data.Time                      (getCurrentTime, parseTimeM, Day,  addDays
                                                , utctDay

                                                , defaultTimeLocale
                                                )
import qualified Data.Time.Format              as Time
                                                ( formatTime )
import           Data.Maybe                     (maybe,  Maybe(..) )
import           Data.Function                  ((.),  ($)
                                                )
import           Data.Semigroup                 ( Endo
                                                , Semigroup((<>))
                                                )
import qualified Data.Text                     as Text
import           TextShow                       ( TextShow(showt) )
import           Data.Text                      ( Text )
import           Obelisk.Route.Frontend         (RouteToUrl, routeLink, setRoute,  R
                                                , SetRoute
                                                , pattern (:/)
                                                )

import           Shared                         (iFa', elLoginSignup
                                                , loadingScreen
                                                , formatTime
                                                )
import           State                          (Session (..), stageUrl, State (..)  )
import           Client                         (getJournalAll,  request
                                                , getAuthData
                                                )
import           Common.Model                   ( Stats(..)
                                                , Journal(..)
                                                )
import           Common.Model                   ( JournalEvent(..)
                                                , EventUser(..)
                                                , EventApp(..)
                                                )
import           Common.Route                   (FrontendRoute
                                                    ( FrontendRoute_Admin
                                                    )
                                                )
import           Data.Functor                   ( Functor(fmap) )
import Data.Bool (Bool(..))
import Data.Foldable (Foldable(length))
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Servant.Reflex (QParam(..))
import Control.Category ((<<<))
import Data.Default (Default(def))
import Data.Function ((&))
import Text.Show (Show(show))
import Control.Category (Category(id))
import Control.Applicative (Applicative(pure))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Functor (void)
import Control.Monad (when)
import GHC.Base (Ord((>)))
import Data.Word (Word)
import Text.Read (readEither)
import Data.Either.Combinators (mapLeft)
import GHC.Real (fromIntegral)
import Data.Foldable (Foldable(null))
import Data.Traversable (Traversable(sequence))
import Data.Witherable (mapMaybe)
import Palantype.Common (Palantype, SystemLang(..))
import qualified Palantype.EN as EN
import qualified Palantype.DE as DE
import qualified Palantype.Common.Stage as Stage
import Palantype.Common.TH (fromJust)

data JournalReqConfig = JournalReqConfig
  { jrcExcludeAdmin :: Bool
  , jrcFilterPred :: JournalEvent
  } deriving (Generic)

instance ToJSON JournalReqConfig
instance FromJSON JournalReqConfig

journal
    :: forall m t
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadReader (Dynamic t State) m
       , PostBuild t m
       , Prerender t m
       , RouteToUrl (R FrontendRoute) m
       , SetRoute t (R FrontendRoute) m
       )
    => Dynamic t Bool
    -> m ()
journal dynHasLoaded = mdo

    dynState <- ask

    elClass "div" "topmenu" $ do
        elClass "div" "floatLeft" $ do
            domBack <- elClass "span" "icon-link big" $ iFa' "fas fa-arrow-circle-left"
            setRoute $ tag (current dynState <&> view (field @"stRedirectUrl")) $ domEvent Click domBack
        elLoginSignup $ constDyn $ FrontendRoute_Admin :/ ()
        elClass "br" "clearBoth" blank

    let
        logout st = case st ^. field @"stSession" of
          SessionAnon   -> Just $ st ^. field @"stRedirectUrl"
          SessionUser _ -> Nothing
    setRoute $ mapMaybe logout $ updated dynState

    let toQParam = maybe (QParamInvalid "couldn't parse date") QParamSome
        dynStart = toQParam <$> dynMStart
        dynEnd   = toQParam <$> dynMEnd
        dynFilterByVisitor =
          either QParamInvalid (maybe QNone $ QParamSome <<< fromIntegral) <$> dynEId
        dynFilterByUser = maybe QNone QParamSome <$> dynMUser
        dynFilterByAlias = maybe QNone QParamSome <$> dynMAlias

        evParamUpdate = leftmost $ updated <$>
                  [ void dynMStart
                  , void dynMEnd
                  , void dynExclAdmin
                  , void dynEId
                  , void dynMUser
                  , void dynMAlias
                  , void dynFilterAnon
                  ]
        evLoad = leftmost [ gate (current dynHasLoaded) evParamUpdate, void $ updated dynHasLoaded]

    evResp <- request
        $ getJournalAll (getAuthData <$> dynState)
            dynStart
            dynEnd
            dynExclAdmin
            dynFilterByVisitor
            dynFilterByUser
            dynFilterByAlias
            dynFilterAnon
            $ evLoad

    (dynMStart, dynMEnd, dynExclAdmin, dynEId, dynMUser, dynMAlias, dynFilterAnon) <-
      elClass "div" "journal" do
        params <- elClass "div" "filters" $ do
            dynENow <- prerender (pure $ Left "before switchover") $ Right <$> liftIO getCurrentTime

            let
                evEDayEnd = updated dynENow <&> fmap utctDay
                evEDayStart = evEDayEnd <&> fmap (addDays (-7))
                eDayToStr = either id (Text.pack <<< show)
                confStart = def & inputElementConfig_setValue .~ (eDayToStr <$> evEDayStart)
                confEnd   = def & inputElementConfig_setValue .~ (eDayToStr <$> evEDayEnd)

            dynMStart' <- el "span" $ elLabelInputDate confStart "Start " "date-start"
            dynMEnd'   <- el "span" $ elLabelInputDate confEnd   "End " "date-end"

            let elemId = "exclude-admin"
                confCbx = def
                  & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "checkbox" <> "id" =: elemId)
                  & inputElementConfig_initialChecked .~ True
            dynExclAdmin' <- el "span" do
              domCbx <- inputElement confCbx
              elAttr "label" ("for" =: elemId) $ el "span" $ do
                text "Exclude admin user "
                el "em" $ dynText $ dynState <&>
                  view ( field @"stSession"
                       . _As @"SessionUser"
                       . field @"sdUserName"
                       )
                text " "
              pure $ _inputElement_checked domCbx

            dynEId' <- el "span" $ elLabelInputWord def "Visitor ID " "filter-visitor"

            dynMUser' <- el "span" $ elLabelInput def "User name " "filter-user"

            dynMAlias' <- el "span" $ elLabelInput def "Alias " "filter-alias"

            let elemIdAnon = "filter-anonymous"
                confCbxAnon = def &
                  inputElementConfig_elementConfig
                  . elementConfig_initialAttributes
                  .~ ("type" =: "checkbox" <> "id" =: elemIdAnon)
            dynFilterAnon' <- el "span" do
              domCbx <- inputElement confCbxAnon
              elAttr "label" ("for" =: elemIdAnon) $ el "span" $ do
                text "Show only anonymous "
              pure $ _inputElement_checked domCbx

            pure
              ( dynMStart'
              , dynMEnd'
              , dynExclAdmin'
              , dynEId'
              , dynMUser'
              , dynMAlias'
              , dynFilterAnon'
              )

        el "hr" blank

        widgetHold_ (loadingScreen "Checking your access rights ...") $ evResp <&> \case

            Left strErr     ->
              elClass "p" "red small" $
                text $ "Error: " <> strErr

            Right lsJournal -> do

              let n = length lsJournal
              el "p" $ text $ "Fetched " <> showt n <> " journal entries."
              when (n > 0) $ el "table" $ do
                  el "tr" do
                      el "th" $ text "Time"
                      el "th" $ text "User"
                      el "th" $ text "Alias"
                      el "th" $ text "VID"
                      el "th" $ text "Event"
                  for_ lsJournal \Journal {..} -> el "tr" do
                      elClass "td" "date" $ text $ Text.pack $ Time.formatTime defaultTimeLocale "%F %R" journalTime
                      case journalMAliasUser of
                          Just (alias, user) -> do
                              el "td" $ text user
                              el "td" $ text alias
                          Nothing -> elAttr "td" ("colspan" =: "2") $ el "em" $ text "anonymous"
                      elAttr "td" ("title" =: journalVisitorIp)$ text $ showt journalVisitorId
                      el "td" $ showEvent journalEvent

        pure params
    blank

showEvent
  :: forall (m :: * -> *) t
  . ( DomBuilder t m
    , Prerender t m
    , RouteToUrl (R FrontendRoute) m
    , SetRoute t (R FrontendRoute) m
    )
  => JournalEvent
  -> m ()
showEvent = \case
    EventUser eu -> text case eu of
        EventLogin          -> "login"
        EventLogout         -> "logout"
        EventSignup         -> "signup"
        EventEdit x old new -> "edit " <> x <> ": " <> old <> " → " <> new
        EventDelete         -> "delete"
    EventApp ea -> case ea of
        EventViewPage path -> do
          text "page view "
          elAttr "a" ("href" =: path) $ text path

        EventStageCompleted lang stageRepr Stats {..} -> do
            text "stage completed "
            let
                mkRouteLink :: forall key. Palantype key => m ()
                mkRouteLink = do
                    let (r, str) = $fromJust $ do
                          index <- Stage.findStageIndex stageRepr
                          stage <- Stage.fromIndex @key index
                          pure $ (stageUrl @key index, showt stage)
                    routeLink r $ text str

            case lang of
                SystemEN -> mkRouteLink @EN.Key
                SystemDE -> mkRouteLink @DE.Key

            text $ " " <> formatTime statsTime

elLabelInputDate
    :: DomBuilder t m
    => InputElementConfig e t (DomBuilderSpace m)
    -> Text
    -> Text
    -> m ( Dynamic t (Maybe Day))
elLabelInputDate conf label elemId = do
    elAttr "label" ("for" =: elemId) $ text label
    i <-
        inputElement
        $  conf & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("id" =: elemId <> "type" =: "date")
    pure $ _inputElement_value i <&> \str ->
          parseTimeM True defaultTimeLocale "%Y-%m-%d" (Text.unpack str)

elLabelInput
    :: DomBuilder t m
    => InputElementConfig e t (DomBuilderSpace m)
    -> Text
    -> Text
    -> m ( Dynamic t (Maybe Text))
elLabelInput conf label elemId = do
    elAttr "label" ("for" =: elemId) $ text label
    i <- inputElement $
      conf & inputElementConfig_elementConfig
           . elementConfig_initialAttributes
           .~ ("id" =: elemId <> "type" =: "text" <> "maxlength" =: "64")
    pure $ _inputElement_value i <&> \str ->
      if Text.null str then Nothing else Just str

elLabelInputWord
    :: DomBuilder t m
    => InputElementConfig e t (DomBuilderSpace m)
    -> Text
    -> Text
    -> m ( Dynamic t (Either Text (Maybe Word)))
elLabelInputWord conf label elemId = do
    elAttr "label" ("for" =: elemId) $ text label
    i <- inputElement $
      conf & inputElementConfig_elementConfig
           . elementConfig_initialAttributes
           .~ ("id" =: elemId <> "type" =: "number" <> "patern" =: "\\d+")
    let toMaybe str = if null str then Nothing else Just str
    pure $ sequence
         . fmap (mapLeft Text.pack . readEither)
         . toMaybe
         . Text.unpack
         <$> _inputElement_value i
