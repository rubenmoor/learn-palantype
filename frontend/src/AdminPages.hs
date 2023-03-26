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

import           Control.Lens.Getter            ( (^.)
                                                , view
                                                )
import           Control.Lens.Setter            ( (.~) )
import           Control.Monad.Reader           ( MonadReader(ask) )
import           Data.Either                    ( Either(..)
                                                , either
                                                )
import           Data.Foldable                  ( Foldable(length, null)
                                                , for_
                                                )
import           Data.Function                  ( ($)
                                                , (&)
                                                , (.)
                                                )
import           Data.Functor                   ( (<$>)
                                                , (<&>)
                                                , Functor(fmap)
                                                , void
                                                )
import           Data.Generics.Product          ( field )
import           Data.Generics.Sum              ( _As )
import           Data.Maybe                     ( Maybe(..)
                                                , maybe
                                                )
import           Data.Semigroup                 ( Endo
                                                , Semigroup((<>))
                                                )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           Data.Time                      ( Day
                                                , addDays
                                                , defaultTimeLocale
                                                , getCurrentTime
                                                , parseTimeM
                                                , utctDay
                                                )
import qualified Data.Time.Format              as Time
                                                ( formatTime )
import           Obelisk.Route.Frontend         ( pattern (:/)
                                                , R
                                                , RouteToUrl
                                                , SetRoute
                                                , routeLink
                                                , setRoute
                                                )
import           Reflex.Dom                     ( (=:)
                                                , DomBuilder
                                                , DomBuilderSpace
                                                , Dynamic
                                                , EventName(Click)
                                                , EventWriter
                                                , InputElement(..)
                                                , InputElementConfig
                                                , MonadHold
                                                , PostBuild
                                                , Prerender
                                                , blank
                                                , constDyn
                                                , current
                                                , domEvent
                                                , dynText
                                                , el
                                                , elAttr
                                                , elClass
                                                , elementConfig_initialAttributes
                                                , gate
                                                , inputElement
                                                , inputElementConfig_elementConfig
                                                , inputElementConfig_initialChecked
                                                , inputElementConfig_setValue
                                                , leftmost
                                                , prerender
                                                , tag
                                                , text
                                                , updated
                                                , widgetHold_
                                                )
import           TextShow                       ( TextShow(showt) )

import           Client                         ( getAuthData
                                                , getJournalAll
                                                , request
                                                )
import           Common.Model                   ( EventApp(..)
                                                , EventUser(..)
                                                , Journal(..)
                                                , JournalEvent(..)
                                                , Stats(..)
                                                )
import           Common.Route                   ( FrontendRoute
                                                    ( FrontendRoute_Admin
                                                    ), FrontendRoute_AdminPages (AdminPage_Journal)
                                                )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( (<<<)
                                                , Category(id)
                                                )
import           Control.Monad                  ( when )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Bool                      ( Bool(..) )
import           Data.Default                   ( Default(def) )
import           Data.Either.Combinators        ( mapLeft )
import           Data.Word                      ( Word )
import           GHC.Base                       ( Ord((>)) )
import           GHC.Generics                   ( Generic )
import           GHC.Real                       ( fromIntegral )
import           Palantype.Common               ( Palantype
                                                , SystemLang(..)
                                                )
import qualified Palantype.Common.Stage        as Stage
import           Palantype.Common.TH            ( fromJust )
import qualified Palantype.DE                  as DE
import qualified Palantype.EN                  as EN
import           Servant.Reflex                 ( QParam(..) )
import           Shared                         ( elLoginSignup
                                                , formatTime
                                                , iFa'
                                                , elLoading
                                                )
import           State                          ( Session(..)
                                                , State(..)
                                                , stageUrl
                                                )
import           Text.Read                      ( readEither )
import           Text.Show                      ( Show(show) )
import           Witherable                     ( mapMaybe )
import Data.Traversable (Traversable(traverse))
import Control.Lens (At(at), (?~), (%~), non)

data JournalReqConfig = JournalReqConfig
    { jrcExcludeAdmin :: Bool
    , jrcFilterPred   :: JournalEvent
    }
    deriving Generic

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

    elClass "div" "shadow-md p-1" $ do
        elClass "div" "float-left" $ do
            domBack <- elClass "span" "text-zinc-500 hover:text-grayishblue-800 text-3xl \
                               \cursor-pointer" $ iFa' "fas fa-arrow-circle-left"
            setRoute $ tag (current dynState <&> view (field @"stRedirectUrl"))
              $ domEvent Click domBack
        elLoginSignup $ constDyn $ FrontendRoute_Admin :/ AdminPage_Journal :/ ()
        elClass "br" "clear-both" blank

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
            dynFilterAnon evLoad

    (dynMStart, dynMEnd, dynExclAdmin, dynEId, dynMUser, dynMAlias, dynFilterAnon) <-
      elClass "div" "p-4" do
        params <- elClass "div" "flex justify-center items-center flex-wrap" $ do
            dynENow <- prerender (pure $ Left "before switchover") $ Right <$> liftIO getCurrentTime

            let
                setClass = inputElementConfig_elementConfig
                         . elementConfig_initialAttributes
                         . at "class" . non "" %~ (<> " p-0 rounded my-1 mr-2")
                evEDayEnd = updated dynENow <&> fmap utctDay
                evEDayStart = evEDayEnd <&> fmap (addDays (-7))
                eDayToStr = either id (Text.pack <<< show)
                confStart = def & inputElementConfig_setValue .~ (eDayToStr <$> evEDayStart)
                                & setClass
                confEnd   = def & inputElementConfig_setValue .~ (eDayToStr <$> evEDayEnd)
                                & setClass

            dynMStart' <- el "span" $ elLabelInputDate confStart "Start " "date-start"
            dynMEnd'   <- el "span" $ elLabelInputDate confEnd   "End "   "date-end"

            let elemId = "exclude-admin"
                confCbx = def
                  & inputElementConfig_elementConfig
                  . elementConfig_initialAttributes
                  %~ (at "type" ?~ "checkbox") . (at "id" ?~ elemId)
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

            dynEId' <- el "span" $ elLabelInputWord (setClass def) "Visitor ID " "filter-visitor"

            dynMUser' <- el "span" $ elLabelInput (setClass def) "User name " "filter-user"

            dynMAlias' <- el "span" $ elLabelInput (setClass def) "Alias " "filter-alias"

            let elemIdAnon = "filter-anonymous"
                confCbxAnon = def
                  & inputElementConfig_elementConfig
                  . elementConfig_initialAttributes
                  .~ "type" =: "checkbox" <> "id" =: elemIdAnon
            dynFilterAnon' <- el "span" do
              domCbx <- inputElement confCbxAnon
              elAttr "label" ("for" =: elemIdAnon) $ el "span" $
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

        widgetHold_ (elLoading "Checking your access rights ...") $ evResp <&> \case

            Left  strErr    -> elClass "p" "text-red-500 text-xs"
                $ text $ "Error: " <> strErr

            Right lsJournal -> do

              let n = length lsJournal
              elClass "p" "text-xs" $ el "em" $ text $ "Fetched " <> showt n <> " journal entries."
              when (n > 0) $ el "table" $ do
                  el "tr" do
                      elClass "th" "text-left" $ text "Time"
                      elClass "th" "text-left" $ text "User"
                      elClass "th" "text-left" $ text "Alias"
                      elClass "th" "text-left" $ text "VID"
                      elClass "th" "text-left" $ text "Event"
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
                          pure (stageUrl @key index, showt stage)
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
        $  conf & inputElementConfig_elementConfig
                . elementConfig_initialAttributes
                %~ (at "id" ?~ elemId) . (at "type" ?~ "date")
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
           %~ (at "id" ?~ elemId) . (at "type" ?~ "text") . (at "maxlength" ?~ "64")
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
           %~ (at "id" ?~ elemId) . (at "type" ?~ "number") . (at "pattern" ?~ "\\d+")
    let toMaybe str = if null str then Nothing else Just str
    pure $ traverse (mapLeft Text.pack . readEither) . toMaybe
         . Text.unpack
         <$> _inputElement_value i
