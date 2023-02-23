{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module GithubApi where

import Network.Wreq (statusMessage, responseBody, statusCode, responseStatus, auth, oauth2Token, getWith, header, defaults)
import Data.Text (Text)
import Common.Model (TextLang)
import Palantype.Common (SystemLang)
import Servant (ToHttpApiData(toUrlPiece))
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Text as Text
import Data.Function ((&))
import Control.Lens ((^.), (.~))

import AppData (EnvApplication (..), Handler)
import qualified Data.Text.Encoding as Text
import Control.Monad.Reader (MonadReader(ask))
import Data.Aeson.Lens (key, _String)

data RequestData = RequestData
  { rdSystemLang :: SystemLang
  , rdTextLang :: TextLang
  , rdPageName :: Text
  }

data Response
  = Success Text
  | Error   Int Text

request :: RequestData -> Handler Response
request RequestData{..} = do
    EnvApplication {..} <- ask
    let
        url = "https://api.github.com/repos/rubenmoor/learn-palantype/main/cms-content/"
           <> toUrlPiece rdSystemLang <> "/"
           <> toUrlPiece rdTextLang <> "/"
           <> rdPageName <> ".md"
        mAuth = if Text.null envGithubToken
                then Nothing
                else Just $ oauth2Token (Text.encodeUtf8 envGithubToken)
        opts = defaults & header "Accept"               .~ ["application/vnd.github.raw"]
                        & header "X-GitHub-Api-Version" .~ ["2022-11-28"                ]
                        & auth .~ mAuth
    resp <- liftIO $ getWith opts $ Text.unpack url
    pure $ case resp ^. responseStatus . statusCode of
        200  -> Success $ resp ^. responseBody . key "content" . _String
        code -> Error code $ Text.decodeUtf8 $ resp ^. responseStatus . statusMessage
