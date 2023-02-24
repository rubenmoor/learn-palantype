{-# LANGUAGE DataKinds #-}

module Handler.CMS where

import           AppData                        ( Handler )
import           Common.Api                     ( RoutesCMS )
import           Common.Model                   ( TextLang )
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.ByteString.Lazy.UTF8     as BLU
import           Data.Default                   ( Default(def) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy                as LazyText
import qualified Data.Text.Lazy.Encoding       as LazyText
import qualified GithubApi
import           Palantype.Common               ( SystemLang )
import           Servant.Server                 ( HasServer(ServerT)
                                                , ServantErr(errBody)
                                                , err400
                                                , err500
                                                , throwError
                                                )
import qualified Text.Pandoc.Class             as Pandoc
import           Text.Pandoc.Class              ( PandocIO )
import           Text.Pandoc.Definition         ( Pandoc )
import qualified Text.Pandoc.Readers           as Pandoc
import qualified Data.ByteString.Lazy as BL

separatorToken :: Text
separatorToken = "<!--separator-->"

handlers :: ServerT RoutesCMS a Handler
handlers = handleCMS

handleCMS
    :: Maybe SystemLang -> Maybe TextLang -> Maybe Text -> Handler [Pandoc]
handleCMS Nothing _ _ = bail "SystemLang"
handleCMS _ Nothing _ = bail "TextLang"
handleCMS _ _ Nothing = bail "PageName"
handleCMS (Just systemLang) (Just textLang) (Just pageName) =
    GithubApi.request (GithubApi.RequestData systemLang textLang pageName)
        >>= \case
                GithubApi.Success str ->
                    liftIO (Pandoc.runIO $ convertMarkdown str) >>= \case
                        Left err -> throwError $ err500
                            { errBody = "Couldn't convert markdown"
                                            <> BLU.fromString (show err)
                            }
                        Right ls -> pure ls
                GithubApi.Error code msg -> throwError $ err500
                    { errBody = "Couldn't retrieve page from CMS: "
                                <> BLU.fromString (show code)
                                <> " "
                                <> LazyText.encodeUtf8 (LazyText.fromStrict msg)
                    }

convertMarkdown :: Text -> PandocIO [Pandoc]
convertMarkdown str =
    traverse (Pandoc.readMarkdown def) $ Text.splitOn separatorToken str

bail :: forall a. BL.ByteString -> Handler a
bail p = throwError $ err400 { errBody = "QueryParam not optional: " <> p }
