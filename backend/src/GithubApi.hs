{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module GithubApi
  ( Response (..)
  , getTextFile
  , getFileList
  , getBlogFiles
  ) where

import           Common.Model                   ( TextLang )
import           Control.Lens                   ( (.~)
                                                , (^.), (^..), Prism, prism, each, filteredBy, only, filtered
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
import Data.Aeson.Lens (key, _String, _Array)
import Data.Aeson (object, Value, KeyValue ((.=)))
import Data.Bool (bool)

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

getBlogFiles :: Handler (Response [Text])
getBlogFiles = do
    mAuth <- asks toMAuth

    lsEContents <- getFileListBlog >>= \case
      Success files     -> liftIO $ traverse (getFileContents mAuth) files
      Error   code  msg -> pure [ Left (code, msg)]

    pure $ case sequence lsEContents of
      Left (code, msg) -> Error code msg
      Right strs       -> Success strs

  where
    getFileContents :: Maybe Auth -> Text -> IO (Either (Int, Text) Text)
    getFileContents mAuth filepath = do
      -- getWith throws exceptions :(
      let url = Text.unpack $ baseurl <> filepath
      try (getWith (myOpts & auth .~ mAuth) url) <&> \case
        Left (HttpExceptionRequest _ (StatusCodeException resp _))
          | responseStatus resp == status404  -> Left (404, filepath)
        Left (HttpExceptionRequest _ content) -> Left (500, Text.pack $ show content)
        Left (InvalidUrlException u msg)      -> Left (500, Text.pack u <> " invalid: " <> Text.pack msg)
        Right resp ->
          let body = LazyText.toStrict $ LazyText.decodeUtf8With lenientDecode $ resp ^. responseBody
          in  if Text.null body
              then Left (400, "Empty response body")
              else Right body

    baseurl =
        "https://api.github.com/repos/rubenmoor/learn-palantype/contents/"

_MarkdownFile :: Text -> Prism Value Value Text Text
_MarkdownFile ext = prism fromFile toFile
  where
    -- not needed in practice, is there such a thing as half a prism?
    fromFile str = object
      [ "type" .= ("blob" :: Text)
      , "path" .= ("cms-content" <> str)
      ]
    toFile   o   = case o ^. key "type" . _String of
      "blob" -> let path = o ^. key "path" . _String
                in  bool (Left o) (Right path) $ checkStr path
      _ -> Left o
    checkStr str =
         "cms-content/" `Text.isPrefixOf` str
      && ext            `Text.isSuffixOf` str

getFileListBlog :: Handler (Response [Text])
getFileListBlog = do
    mAuth <- asks toMAuth
    let
        url = "https://api.github.com/repos/rubenmoor/learn-palantype/git/trees/main?recursive=1"

    liftIO (try $ getWith (myOpts & auth .~ mAuth) $ Text.unpack url) <&> \case

      Left (HttpExceptionRequest _ content) ->
        Error 500 $ Text.pack $ show content

      Left (InvalidUrlException u msg)      ->
        Error 500 $ "Url " <> Text.pack u <> " invalid: " <> Text.pack msg

      Right resp                            ->
        Success $ resp ^.. responseBody . key "tree" . _Array . each
          . filteredBy (key "type" . _String . only "blob")
          . key "path" . _String . filtered checkStr
  where
    checkStr str = "cms-content/blog/" `Text.isPrefixOf` str

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
        Success $ resp ^.. responseBody . key "tree" . _Array . each
          . filteredBy (key "type" . _String . only "blob")
          . key "path" . _String . filtered checkStr
  where
    checkStr str =
         "cms-content/" `Text.isPrefixOf` str
      && ext            `Text.isSuffixOf` str
