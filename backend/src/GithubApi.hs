{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module GithubApi
  ( Response (..)
  , getTextFile
  , getFileList
  ) where

import           Common.Model                   ( TextLang )
import           Control.Lens                   ( (.~)
                                                , (^.), (^..), Prism, prism
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
                                                , responseBody, Options, Auth
                                                )
import           Palantype.Common               ( SystemLang )
import           Servant                        ( ToHttpApiData(toUrlPiece) )

import           AppData                        ( EnvApplication(..)
                                                , Handler
                                                )
import           Control.Monad.Reader           ( asks )
import qualified Data.Text.Encoding            as Text
import           Data.Text.Encoding.Error       ( lenientDecode )
import qualified Data.Text.Lazy                as LazyText
import qualified Data.Text.Lazy.Encoding       as LazyText
import Control.Exception (try)
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (StatusCodeException), responseStatus)
import Data.Functor ((<&>))
import Network.HTTP.Types (status404)
import Data.Aeson.Lens (key, _String)
import Data.Aeson (object, Value, KeyValue ((.=)))

data Response t
  = Success      t
  | Error        Int Text

myOpts :: Options
myOpts = defaults
    &  header "Accept" .~ ["application/vnd.github.raw"]
    &  header "X-GitHub-Api-Version" .~ ["2022-11-28"]

toMAuth :: EnvApplication -> Maybe Auth
toMAuth env = if Text.null $ envGithubToken env
  then Nothing
  else Just $ oauth2Token $ Text.encodeUtf8 $ envGithubToken env

getTextFile :: SystemLang -> TextLang -> Text -> Handler (Response Text)
getTextFile systemLang textLang filename = do
    mAuth <- asks toMAuth
    let
        filepath = "cms-content/"
          <> toUrlPiece systemLang
          <> "/"
          <> toUrlPiece textLang
          <> "/"
          <> filename
        url =
            "https://api.github.com/repos/rubenmoor/learn-palantype/contents/"
          <> filepath
    -- getWith throws exceptions :(
    liftIO (try $ getWith (myOpts & auth .~ mAuth) $ Text.unpack url) <&> \case
      Left (HttpExceptionRequest _ (StatusCodeException resp _)) | responseStatus resp == status404 ->
        Error 404 filepath
      Left (HttpExceptionRequest _ content) -> Error 500 $ Text.pack $ show content
      Left (InvalidUrlException u msg) -> Error 500 $ "Url " <> Text.pack u <> " invalid: " <> Text.pack msg
      Right resp ->
        let body = LazyText.toStrict $ LazyText.decodeUtf8With lenientDecode $ resp ^. responseBody
        in  if Text.null body
            then Error 400 "Empty response body"
            else Success body

_MarkdownFile :: Text -> Prism Value Value Text Text
_MarkdownFile ext = prism fromFile toFile
  where
    -- not needed in practice, is there such a thing as half a prism?
    fromFile str = object
      [ "type" .= ("blob" :: Text)
      , "path" .= ("cms-content" <> str)
      ]
    toFile   o   = case o ^. key "type" . _String of
      "blob" -> case checkStr $ o ^. key "path" . _String of
        Just str -> Right str
        Nothing  -> Left o
      _ -> Left o
    checkStr str = if "cms-content/" `Text.isPrefixOf` str
      then Text.stripSuffix ext str
      else Nothing

getFileList :: Text -> Handler (Response [Text])
getFileList ext = do
    mAuth <- asks toMAuth
    let
        url = "https://api.github.com/repos/rubenmoor/learn-palantype/git/trees/main?recursive=1"

    liftIO (try $ getWith (myOpts & auth .~ mAuth) $ Text.unpack url) <&> \case

      Left (HttpExceptionRequest _ content) ->
        Error 500 $ Text.pack $ show content

      Left (InvalidUrlException u msg)      ->
        Error 500 $ "Url " <> Text.pack u <> " invalid: " <> Text.pack msg
      Right resp                            ->
        Success $ resp ^.. responseBody . key "tree" . _MarkdownFile ext
