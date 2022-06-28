{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BlockArguments #-}

module AdminPages where

import Reflex.Dom ((=:), elAttr, DomBuilder, MonadHold, elClass, widgetHold_, text, el, Prerender, Dynamic, PostBuild, getPostBuild, blank)
import Control.Monad.Reader (MonadReader(ask))
import Data.Foldable (for_)
import Data.Functor ((<$>), (<&>))
import Data.Either (Either(..))
import Data.Time ( defaultTimeLocale )
import qualified Data.Time.Format as Time (formatTime)
import Data.Maybe (Maybe(..))
import Data.Function (($))
import Data.Semigroup (Semigroup((<>)))
import qualified Data.Text as Text
import TextShow (TextShow(showt))
import Data.Text (Text)

import Shared (loadingScreen, formatTime)
import State (State)
import Client ( request, getAuthData, getJournalAll )
import Common.Model (Stats (..), Journal(..))
import Common.Model (Event (..), EventUser (..), EventApp (..))

journal
  :: forall m t.
     ( DomBuilder t m
     , MonadHold t m
     , MonadReader (Dynamic t State) m
     , PostBuild t m
     , Prerender t m
     )
  => m ()
journal = do
    evPb <- getPostBuild
    dynState <- ask
    evResp <- request $ getJournalAll (getAuthData <$> dynState) evPb
    widgetHold_ (loadingScreen "Checking your access rights ...") $ evResp <&> \case
      Left strErr -> elClass "p" "red small" $
        text $ "Couldn't load resource: " <> strErr
      Right lsJournal -> el "table" $ for_ lsJournal \Journal{..} -> el "tr" do
        el "td" $ text $ Text.pack $ Time.formatTime defaultTimeLocale "%F %R" journalTime
        case journalMAliasUser of
          Just (alias, user) -> do
            el "td" $ text user
            el "td" $ text alias
          Nothing -> elAttr "td" ("colspan" =: "2") $ el "em" $ text "anonymous"
        el "td" $ text $ showt journalVisitorId
        el "td" $ text $ showEvent journalEvent
    blank

showEvent :: Event -> Text
showEvent = \case
  EventUser eu -> case eu of
    EventLogin -> "login"
    EventLogout -> "logout"
    EventSignup -> "signup"
    EventEdit x old new -> "edit " <> x <> ": " <> old <> " → " <> new
    EventDelete -> "delete"
  EventApp ea -> case ea of
    EventViewPage path -> "page view " <> path
    EventStageCompleted stage Stats{..} -> "stage completed " <> showt stage <> formatTime statsTime
