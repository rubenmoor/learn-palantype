{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Handler.CMS where

import Servant.Server (HasServer(ServerT), err500, ServantErr(errBody), throwError)
import Common.Api (RoutesCMS)
import AppData (Handler)
import Data.Text (Text)
import Common.Model (TextLang)
import Palantype.Common (SystemLang)
import Text.Pandoc.Definition (Pandoc)
import qualified GithubApi
import qualified Text.Pandoc.Readers as Pandoc
import qualified Text.Pandoc.Class as Pandoc
import Text.Pandoc.Class (PandocIO)
import qualified Data.Text as Text
import Data.Default (Default (def))
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Control.Monad.IO.Class (liftIO)
import Data.Traversable (traverse)

separatorToken :: Text
separatorToken = "<!--separator-->"

handlers :: ServerT RoutesCMS a Handler
handlers =
  (
         handleCMS
  )

handleCMS :: Maybe SystemLang -> Maybe TextLang -> Maybe Text -> Handler [Pandoc]
handleCMS (Just systemLang) (Just textLang) (Just pageName) =
  GithubApi.request (GithubApi.RequestData systemLang textLang pageName) >>= \case
      GithubApi.Success str ->
        liftIO (Pandoc.runIO $ convertMarkdown str) >>= \case
          Left err -> throwError $ err500
            { errBody = "Couldn't convert markdown" <> BLU.fromString (show err)
            }
          Right ls -> pure ls
      GithubApi.Error   code msg -> throwError $ err500
        { errBody = "Couldn't retrieve page from CMS"
        }

convertMarkdown :: Text -> PandocIO [Pandoc]
convertMarkdown str = traverse (Pandoc.readMarkdown def) $ Text.splitOn separatorToken str
