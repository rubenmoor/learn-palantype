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
import           Common.Model                   ( TextLang, UTCTimeInUrl (UTCTimeInUrl), CacheContentType (..) )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Category               ( (<<<) )
import           Control.Monad                  ( Monad((>>=)), unless )
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.UTF8     as BLU
import           Data.Default                   ( Default(def) )
import           Data.Either                    ( Either(..) )
import           Data.Foldable                  ( traverse_, Foldable (null) )
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
import Data.Int (Int)
import qualified Data.Text.IO as Text

separatorToken :: Text
separatorToken = "<!--separator-->"

handlers :: ServerT RoutesCMS a Handler
handlers =
         handleCMSGet
    :<|> handlePostCacheInvalidation
    :<|> handleGetCacheInvalidationData
    :<|> handleClearCacheAll
    :<|> handleClearCache
    :<|> handlePostUpdateAll

handleCMSGet
  :: SystemLang
  -> TextLang
  -> Text
  -> UTCTimeInUrl
  -> Handler [Pandoc]
handleCMSGet systemLang textLang filename (UTCTimeInUrl time) = do
    modifyResponse $ setHeader "Cache-Control" "public, max-age=31500000, immutable"
    let cacheDbKey = UCMSCache systemLang textLang filename time
    mFromCache <- runDb (getBy cacheDbKey) >>= \case
        Just (Entity _ CMSCache {..}) ->
          case cMSCacheContentType of
            CacheContentMarkdown -> case blobDecode cMSCacheBlob of
              Nothing    -> runDb (deleteBy cacheDbKey) $> Nothing
              Just lsDoc -> pure $ Just lsDoc
            -- TODO: other content types
        Nothing -> pure Nothing
    case mFromCache of
      Just c -> do
        liftIO $ Text.putStrLn "Retrieved page from cache"
        pure c
      Nothing -> GithubApi.getTextFile systemLang textLang filename >>= \case
        GithubApi.Success str -> liftIO (Pandoc.runIO $ convertMarkdown str) >>= \case
          Left err -> Servant.throwError $ err500
              { errBody =
                  "Couldn't convert markdown"
                      <> BLU.fromString (show err)
              }
          Right lsDoc -> do
            liftIO $ Text.putStrLn "Fetched from GitHub"
            runDb $ Esqueleto.delete $ from $ \c ->
              where_ $ c ^. CMSCacheSystemLang ==. val systemLang
                   &&. c ^. CMSCacheTextLang   ==. val textLang
                   &&. c ^. CMSCacheFilename   ==. val filename
            _ <- runDb $ insert $ CMSCache systemLang
                                           textLang
                                           filename
                                           CacheContentMarkdown
                                           (blobEncode lsDoc)
                                           time
            pure lsDoc
        GithubApi.Error 404 strFilename -> Servant.throwError $ err404
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
handlePostCacheInvalidation = traverse_ invalidateCache

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
                  , cMSCacheLatestFilename
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
handleClearCache UserInfo{..} systemLang textLang filename = do
  unless uiIsSiteAdmin $ Servant.throwError err403
  runDb $ Esqueleto.delete $ from $ \c ->
    where_ $ c ^. CMSCacheSystemLang ==. val systemLang
         &&. c ^. CMSCacheTextLang   ==. val textLang
         &&. c ^. CMSCacheFilename   ==. val filename

handlePostUpdateAll :: UserInfo -> Handler ()
handlePostUpdateAll UserInfo{..} = do
    unless uiIsSiteAdmin $ Servant.throwError err403
    GithubApi.getFileList ".md" >>= \case
      GithubApi.Success filepaths ->
        if null filepaths
        then bail (500 :: Int) "empty file list"
        else traverse_ invalidateCache filepaths
      GithubApi.Error code msg -> bail code msg
  where bail code msg = Servant.throwError $ err500
            { errBody = "Error communicating with Github: "
                        <> BLU.fromString (show code)
                        <> " "
                        <> textToLazyBS msg
            }

invalidateCache :: Text -> Handler ()
invalidateCache filepath = do
    (strSystemLang, strTextLang, filename) <- case Text.splitOn "/" filepath of
        ["cms-content", s1, s2, s3] -> pure (s1, s2, s3)
        _ -> bail
                $ "required: cms-content/[systemLang]/[textLang]/[filename]; got: "
                <> textToLazyBS filepath
    systemLang <- case readMaybe $ Text.unpack strSystemLang of
        Nothing -> bail $ "Couldn't parse SystemLang: " <> textToLazyBS strSystemLang
        Just s -> pure s
    textLang <- case readMaybe $ Text.unpack strTextLang of
        Nothing -> bail $ "Couldn't parse TextLang: " <> textToLazyBS strTextLang
        Just s -> pure s
    time <- liftIO getCurrentTime
    runDb $ deleteBy $ UCMSCacheLatest systemLang textLang filename
    void $ runDb $ insert $ CMSCacheLatest time
                                           systemLang
                                           textLang
                                           filename
  where
    bail msg = Servant.throwError $ err400 { errBody = msg }
