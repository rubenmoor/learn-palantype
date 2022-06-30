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
import Control.Lens.Getter ((^.))
import Control.Lens.Setter ((.~))
import           Reflex.Dom                     (current, gate, leftmost, prerender, updated, inputElementConfig_setValue, DomBuilderSpace, InputElementConfig, inputElementConfig_initialChecked, elementConfig_initialAttributes, inputElementConfig_elementConfig, inputElement, dyn_, InputElement (..)








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
import           Data.Functor                   ( (<$>)
                                                , (<&>)
                                                )
import           Data.Either                    (either,  Either(..) )
import           Data.Time                      (getCurrentTime, parseTimeM, Day,  addDays
                                                , utctDay
                                                , UTCTime
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
import           Obelisk.Route.Frontend         ( R
                                                , SetRoute
                                                , pattern (:/)
                                                )

import           Shared                         (elLoginSignup
                                                , loadingScreen
                                                , formatTime
                                                )
import           State                          (State (..)  )
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
import           Common.Route                   ( FrontendRoute
                                                    ( FrontendRoute_Admin
                                                    )
                                                )
import           Reflex.Dom                     ( Reflex(Event) )
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
       , SetRoute t (R FrontendRoute) m
       )
    => Dynamic t Bool
    -> Dynamic t (Either Text UTCTime)
    -> m ()
journal dynHasLoaded _ = mdo
    elClass "div" "topmenu" $ do
        elLoginSignup $ constDyn $ FrontendRoute_Admin :/ ()
        elClass "br" "clearBoth" blank

    dynState <- ask

    let toQParam = maybe (QParamInvalid "couldn't parse date") QParamSome
        dynStart = toQParam <$> dynMStart
        dynEnd   = toQParam <$> dynMEnd
        dynExcludeAdmin = constDyn True
        dynFilterByVisitor = constDyn QNone
        dynFilterByUser = constDyn QNone
        dynFilterByAlias = constDyn QNone

        evLoad = leftmost [ gate (current dynHasLoaded) evParamUpdate, void $ updated dynHasLoaded]

    evResp <- request
        $ getJournalAll (getAuthData <$> dynState)
            dynStart
            dynEnd
            dynExcludeAdmin
            dynFilterByVisitor
            dynFilterByUser
            dynFilterByAlias
            $ evLoad

    (dynMStart, dynMEnd, evParamUpdate) <-
      elClass "div" "journal" do
        params <- elClass "div" "filters" $ do
            dynENow <- prerender (pure $ Left "before switchover") $ Right <$> liftIO getCurrentTime
            -- dyn $ dynENow <&> \eNow -> do
            let
                evEDayEnd = updated dynENow <&> fmap utctDay
                evEDayStart = evEDayEnd <&> fmap (addDays (-7))
                eDayToStr = either id (Text.pack <<< show)
                confStart = def & inputElementConfig_setValue .~ (eDayToStr <$> evEDayStart)
                confEnd   = def & inputElementConfig_setValue .~ (eDayToStr <$> evEDayEnd)
            (dynMStart', iStart) <- el "span" $ elLabelInputDate confStart "Start " "date-start"
            (dynMEnd'  , iEnd) <- el "span" $ elLabelInputDate confEnd   "End " "date-end"

            dyn_ $ dynState <&> \State{..} -> do
              let userName = stSession ^. _As @"SessionUser" . field @"sdUserName"
              el "span" $ elLabelCheckbox True userName "exclude-admin"

            pure
              ( dynMStart'
              , dynMEnd'
              , void $ leftmost $ updated <$>
                  [ _inputElement_value iStart
                  , _inputElement_value iEnd
                  ]
              )

        el "hr" blank

        widgetHold_ (loadingScreen "Checking your access rights ...") $ evResp <&> \case

            Left strErr     ->
              elClass "p" "red small" $
                text $ "Couldn't load resource: " <> strErr

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
                      el "td" $ text $ Text.pack $ Time.formatTime defaultTimeLocale "%F %R" journalTime
                      case journalMAliasUser of
                          Just (alias, user) -> do
                              el "td" $ text user
                              el "td" $ text alias
                          Nothing -> elAttr "td" ("colspan" =: "2") $ el "em" $ text "anonymous"
                      el "td" $ text $ showt journalVisitorId
                      el "td" $ text $ showEvent journalEvent

        pure params
    blank

showEvent :: JournalEvent -> Text
showEvent = \case
    EventUser eu -> case eu of
        EventLogin          -> "login"
        EventLogout         -> "logout"
        EventSignup         -> "signup"
        EventEdit x old new -> "edit " <> x <> ": " <> old <> " → " <> new
        EventDelete         -> "delete"
    EventApp ea -> case ea of
        EventViewPage path -> "page view " <> path
        EventStageCompleted stage Stats {..} ->
            "stage completed " <> showt stage <> formatTime statsTime

elLabelCheckbox
    :: (DomBuilder t m) => Bool -> Text -> Text -> m (Dynamic t Bool)
elLabelCheckbox initial labelUserName elemId = do
    cb <-
        inputElement
        $  def
        &  inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "checkbox" <> "id" =: elemId)
        &  inputElementConfig_initialChecked .~ initial
    elAttr "label" ("for" =: elemId) $ el "span" $ do
      text "Exclude admin user "
      el "em" $ text labelUserName
    pure $ _inputElement_checked cb

elLabelInputDate
    :: DomBuilder t m
    => InputElementConfig e t (DomBuilderSpace m)
    -> Text
    -> Text
    -> m
           ( Dynamic t (Maybe Day)
           , InputElement e (DomBuilderSpace m) t
           )
elLabelInputDate conf label elemId = do
    elAttr "label" ("for" =: elemId) $ text label
    i <-
        inputElement
        $  conf & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("id" =: elemId <> "type" =: "date")
    let dynMDay  = _inputElement_value i <&> \str ->
          parseTimeM True defaultTimeLocale "%Y-%m-%d" (Text.unpack str)
    pure (dynMDay, i)
