{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Handler.CMS
    ( handlers
    ) where

import           AppData                        ( Handler )
import           Common.Api                     ( RoutesCMS )
import           Common.Model                   ( TextLang )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( (<<<) )
import           Control.Monad                  ( Monad((>>=)), unless )
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.UTF8     as BLU
import           Data.Default                   ( Default(def) )
import           Data.Either                    ( Either(..) )
import           Data.Foldable                  ( for_ )
import           Data.Function                  ( ($), (&) )
import           Data.Functor                   ( ($>)
                                                , (<&>)
                                                , Functor((<$))
                                                , void
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( Maybe(..) )
import           Data.Semigroup                 ( Semigroup((<>)) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy                as LazyText
import qualified Data.Text.Lazy.Encoding       as LazyText
import           Data.Time                      ( UTCTime
                                                , getCurrentTime
                                                )
import           Data.Traversable               ( Traversable(traverse) )
import           Database                       ( blobDecode
                                                , blobEncode
                                                , runDb
                                                )
import           Database.Gerippe               ( Entity(..)
                                                , PersistUniqueWrite(deleteBy)
                                                , getAll
                                                , getBy
                                                , insert, deleteAll
                                                )
import           DbAdapter                      ( CMSCache ( ..)
                                                , CMSCacheInvalidation
                                                    ( CMSCacheInvalidation
                                                    , cMSCacheInvalidationPageName
                                                    , cMSCacheInvalidationSystemLang
                                                    , cMSCacheInvalidationTextLang
                                                    , cMSCacheInvalidationTime
                                                    )
                                                , Unique(UPage, UPageContent)
                                                )
import qualified GithubApi
import           Palantype.Common               ( SystemLang )
import qualified Servant
import           Servant.API                    ( type (:<|>)((:<|>)) )
import           Servant.Server                 ( HasServer(ServerT)
                                                , ServantErr(errBody)
                                                , err400
                                                , err404
                                                , err500, err403
                                                )
import qualified Text.Pandoc.Class             as Pandoc
import           Text.Pandoc.Class              ( PandocIO )
import           Text.Pandoc.Definition         ( Pandoc )
import Text.Pandoc.Extensions (pandocExtensions)
import qualified Text.Pandoc.Readers           as Pandoc
import           Text.Read                      ( readMaybe )
import           Text.Show                      ( Show(..) )
import Auth (UserInfo (..))
import Data.Generics.Product (field)
import Control.Lens ((.~))

separatorToken :: Text
separatorToken = "<!--separator-->"

handlers :: ServerT RoutesCMS a Handler
handlers =
         handleCMSGet
    :<|> handleInvalidateCache
    :<|> handleGetCacheInvalidationData
    :<|> handleClearCacheAll
    :<|> handleClearCache

handleCMSGet :: SystemLang -> TextLang -> Text -> Handler (UTCTime, [Pandoc])
handleCMSGet systemLang textLang pageName = do
    let cacheDbKey = UPageContent systemLang textLang pageName
    mFromCache <- runDb (getBy cacheDbKey) >>= \case
        Just (Entity _ CMSCache {..}) -> case blobDecode cMSCacheBlob of
            Nothing    -> runDb (deleteBy cacheDbKey) $> Nothing
            Just lsDoc -> pure $ Just (cMSCacheTime, lsDoc)
        Nothing -> pure Nothing
    case mFromCache of
        Just c -> pure c
        Nothing -> GithubApi.request (GithubApi.RequestData systemLang textLang pageName) >>= \case
            GithubApi.Success str -> do
                liftIO (Pandoc.runIO $ convertMarkdown str) >>= \case
                    Left err -> Servant.throwError $ err500
                        { errBody =
                            "Couldn't convert markdown"
                                <> BLU.fromString (show err)
                        }
                    Right lsDoc -> do
                      now <- liftIO getCurrentTime
                      _ <- runDb $ insert $ CMSCache systemLang
                                                     textLang
                                                     pageName
                                                     (blobEncode lsDoc)
                                                     now
                      pure (now, lsDoc)
            GithubApi.PageNotFound strFilename -> Servant.throwError $ err404
                { errBody = "Missing file: " <> textToLazyBS strFilename
                }
            GithubApi.Error code msg -> Servant.throwError $ err500
                { errBody = "Couldn't retrieve page: "
                            <> BLU.fromString (show code)
                            <> " "
                            <> textToLazyBS msg
                }

convertMarkdown :: Text -> PandocIO [Pandoc]
convertMarkdown str =
  let opts = def & field @"readerExtensions" .~ pandocExtensions
  in  traverse (Pandoc.readMarkdown opts) $ Text.splitOn separatorToken str

handleInvalidateCache :: Text -> Handler ()
handleInvalidateCache str = for_ (Text.words str) \filepath -> do
    (strSystemLang, strTextLang, pageName) <- case Text.splitOn "/" filepath of
        ["cms-content", s1, s2, s3] -> pure (s1, s2, s3)
        _ -> bail
                $ "required: cms-content/[systemLang]/[textLang]/[pageName]; got: "
                <> textToLazyBS filepath
    systemLang <- case readMaybe $ Text.unpack strSystemLang of
        Nothing ->
            bail $ "Couldn't parse SystemLang: " <> textToLazyBS strSystemLang
        Just s -> pure s
    textLang <- case readMaybe $ Text.unpack strTextLang of
        Nothing ->
            bail $ "Couldn't parse TextLang: " <> textToLazyBS strTextLang
        Just s -> pure s
    time <- liftIO getCurrentTime
    runDb $ deleteBy $ UPage systemLang textLang pageName
    void $ runDb $ insert $ CMSCacheInvalidation time
                                                 systemLang
                                                 textLang
                                                 pageName
    where bail msg = Servant.throwError $ err400 { errBody = msg }

handleGetCacheInvalidationData
    :: Handler (Map (SystemLang, TextLang, Text) UTCTime)
handleGetCacheInvalidationData = do
    cacheData <- runDb getAll
    pure
        $   Map.fromList
        $   cacheData
        <&> \(Entity _ CMSCacheInvalidation {..}) ->
                ( ( cMSCacheInvalidationSystemLang
                  , cMSCacheInvalidationTextLang
                  , cMSCacheInvalidationPageName
                  )
                , cMSCacheInvalidationTime
                )

textToLazyBS :: Text -> BL.ByteString
textToLazyBS = LazyText.encodeUtf8 <<< LazyText.fromStrict

handleClearCacheAll :: UserInfo -> Handler ()
handleClearCacheAll UserInfo{..} = do
  unless uiIsSiteAdmin $ Servant.throwError err403
  runDb $ deleteAll @CMSCache

handleClearCache :: UserInfo -> SystemLang -> TextLang -> Text -> Handler ()
handleClearCache UserInfo{..} systemLang textLang pageName = do
  unless uiIsSiteAdmin $ Servant.throwError err403
  let cacheDbKey = UPageContent systemLang textLang pageName
  runDb $ deleteBy cacheDbKey
