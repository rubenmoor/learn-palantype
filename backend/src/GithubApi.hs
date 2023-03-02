{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module GithubApi where

import           Common.Model                   ( TextLang )
import           Control.Lens                   ( (.~)
                                                , (^.)
                                                )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Data.Function                  ( (&) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Network.Wreq                   ( auth
                                                , defaults
                                                , getWith
                                                , header
                                                , oauth2Token
                                                , responseBody
                                                , responseStatus
                                                , statusCode
                                                , statusMessage
                                                )
import           Palantype.Common               ( SystemLang )
import           Servant                        ( ToHttpApiData(toUrlPiece) )

import           AppData                        ( EnvApplication(..)
                                                , Handler
                                                )
import           Control.Monad.Reader           ( MonadReader(ask) )
import qualified Data.Text.Encoding            as Text
import           Data.Text.Encoding.Error       ( lenientDecode )
import qualified Data.Text.Lazy                as LazyText
import qualified Data.Text.Lazy.Encoding       as LazyText

data RequestData = RequestData
    { rdSystemLang :: SystemLang
    , rdTextLang   :: TextLang
    , rdPageName   :: Text
    }

data Response
  = Success Text
  | Error   Int Text

request :: RequestData -> Handler Response
request RequestData {..} = do
    EnvApplication {..} <- ask
    let url =
            "https://api.github.com/repos/rubenmoor/learn-palantype/contents/cms-content/"
                <> toUrlPiece rdSystemLang
                <> "/"
                <> toUrlPiece rdTextLang
                <> "/"
                <> rdPageName
                <> ".md"
        mAuth = if Text.null envGithubToken
            then Nothing
            else Just $ oauth2Token (Text.encodeUtf8 envGithubToken)
        opts =
            defaults
                &  header "Accept"
                .~ ["application/vnd.github.raw"]
                &  header "X-GitHub-Api-Version"
                .~ ["2022-11-28"]
                &  auth
                .~ mAuth
    -- TODO: getWith throws exceptions :(
    resp <- liftIO $ getWith opts $ Text.unpack url
    pure $ case resp ^. responseStatus . statusCode of
        200 -> let body = LazyText.toStrict $ LazyText.decodeUtf8With lenientDecode $ resp ^. responseBody
               in  if Text.null body
                      then Error 400 "Empty response body"
                      else Success body
        404  -> Error 404 "Not found: url"
        code ->
            Error code
                $  Text.decodeUtf8
                $  resp ^. responseStatus .  statusMessage
