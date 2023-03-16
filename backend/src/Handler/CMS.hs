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
import           Common.Model                   ( TextLang, UTCTimeInUrl (UTCTimeInUrl) )
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
                                                , deleteAll
                                                , getAll
                                                , getBy
                                                , insert
                                                , from
                                                , val
                                                , where_
                                                , (^.)
                                                , (==.)
                                                , (&&.)
                                                , PersistUniqueWrite(deleteBy)
                                                )
import qualified Database.Esqueleto.Experimental as Esqueleto
import           DbAdapter                      ( CMSCache ( .. )
                                                , CMSCacheLatest ( .. )
                                                , Unique(UCMSCacheLatest, UCMSCache)
                                                , EntityField(..)
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
import Snap.Core (modifyResponse, setHeader)

separatorToken :: Text
separatorToken = "<!--separator-->"

handlers :: ServerT RoutesCMS a Handler
handlers =
         handleCMSGet
    :<|> handlePostCacheInvalidation
    :<|> handleGetCacheInvalidationData
    :<|> handleClearCacheAll
    :<|> handleClearCache

handleCMSGet
  :: SystemLang
  -> TextLang
  -> Text
  -> UTCTimeInUrl
  -> Handler [Pandoc]
handleCMSGet systemLang textLang pageName (UTCTimeInUrl time) = do
    modifyResponse $ setHeader "Cache-Control" "public, max-age=31500000, immutable"
    let cacheDbKey = UCMSCache systemLang textLang pageName time
    mFromCache <- runDb (getBy cacheDbKey) >>= \case
        Just (Entity _ CMSCache {..}) -> case blobDecode cMSCacheBlob of
            Nothing    -> runDb (deleteBy cacheDbKey) $> Nothing
            Just lsDoc -> pure $ Just lsDoc
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
                      runDb $ Esqueleto.delete $ from $ \c ->
                        where_ $ c ^. CMSCacheSystemLang ==. val systemLang
                             &&. c ^. CMSCacheTextLang   ==. val textLang
                             &&. c ^. CMSCachePageName   ==. val pageName
                      _ <- runDb $ insert $ CMSCache systemLang
                                                     textLang
                                                     pageName
                                                     (blobEncode lsDoc)
                                                     time
                      pure lsDoc
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

handlePostCacheInvalidation :: [Text] -> Handler ()
handlePostCacheInvalidation filepaths = for_ filepaths \filepath -> do
    (strSystemLang, strTextLang, pageName) <- case Text.splitOn "/" filepath of
        ["cms-content", s1, s2, s3] -> pure (s1, s2, s3)
        _ -> bail
                $ "required: cms-content/[systemLang]/[textLang]/[pageName]; got: "
                <> textToLazyBS filepath
    systemLang <- case readMaybe $ Text.unpack strSystemLang of
        Nothing -> bail $ "Couldn't parse SystemLang: " <> textToLazyBS strSystemLang
        Just s -> pure s
    textLang <- case readMaybe $ Text.unpack strTextLang of
        Nothing -> bail $ "Couldn't parse TextLang: " <> textToLazyBS strTextLang
        Just s -> pure s
    time <- liftIO getCurrentTime
    runDb $ deleteBy $ UCMSCacheLatest systemLang textLang pageName
    void $ runDb $ insert $ CMSCacheLatest time
                                           systemLang
                                           textLang
                                           pageName
    where bail msg = Servant.throwError $ err400 { errBody = msg }

handleGetCacheInvalidationData
    :: Handler (Map (SystemLang, TextLang, Text) UTCTime)
handleGetCacheInvalidationData = do
    modifyResponse $ setHeader "Cache-Control" "no-store, must-revalidate"
    cacheData <- runDb getAll
    pure
        $   Map.fromList
        $   cacheData
        <&> \(Entity _ CMSCacheLatest {..}) ->
                ( ( cMSCacheLatestSystemLang
                  , cMSCacheLatestTextLang
                  , cMSCacheLatestPageName
                  )
                , cMSCacheLatestTime
                )

textToLazyBS :: Text -> BL.ByteString
textToLazyBS = LazyText.encodeUtf8 <<< LazyText.fromStrict

handleClearCacheAll :: UserInfo -> Handler ()
handleClearCacheAll UserInfo{..} = do
  unless uiIsSiteAdmin $ Servant.throwError err403
  runDb $ deleteAll @CMSCache

handleClearCache
  :: UserInfo
  -> SystemLang
  -> TextLang
  -> Text
  -> Handler ()
handleClearCache UserInfo{..} systemLang textLang pageName = do
  unless uiIsSiteAdmin $ Servant.throwError err403
  runDb $ Esqueleto.delete $ from $ \c ->
    where_ $ c ^. CMSCacheSystemLang ==. val systemLang
         &&. c ^. CMSCacheTextLang   ==. val textLang
         &&. c ^. CMSCachePageName   ==. val pageName
