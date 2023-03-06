{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

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
import Control.Exception (try)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (StatusCodeException), responseStatus)
import Data.Functor ((<&>))
import Network.HTTP.Types (status404)

data RequestData = RequestData
    { rdSystemLang :: SystemLang
    , rdTextLang   :: TextLang
    , rdPageName   :: Text
    }

data Response
  = Success Text
  | PageNotFound Text
  | Error   Int Text

request :: RequestData -> Handler Response
request RequestData {..} = do
    EnvApplication {..} <- ask
    let
        filename = "cms-content/"
          <> toUrlPiece rdSystemLang
          <> "/"
          <> toUrlPiece rdTextLang
          <> "/"
          <> rdPageName
          <> ".md"
        url =
            "https://api.github.com/repos/rubenmoor/learn-palantype/contents/"
          <> filename
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
    -- getWith throws exceptions :(
    liftIO (try $ getWith opts $ Text.unpack url) <&> \case
      Left (HttpExceptionRequest req (StatusCodeException resp msg)) | responseStatus resp == status404 ->
        PageNotFound filename
      Left (HttpExceptionRequest req content) -> Error 500 $ Text.pack $ show content
      Left (InvalidUrlException u msg) -> Error 500 $ "Url " <> Text.pack u <> " invalid: " <> Text.pack msg
      Right resp ->
        let body = LazyText.toStrict $ LazyText.decodeUtf8With lenientDecode $ resp ^. responseBody
        in  if Text.null body
            then Error 400 "Empty response body"
            else Success body

    -- resp <- liftIO (getWith opts $ Text.unpack url)
    -- pure $ let body = LazyText.toStrict $ LazyText.decodeUtf8With lenientDecode $ resp ^. responseBody
    --        in  if Text.null body
    --            then Error 400 "Empty response body"
    --            else Success body
