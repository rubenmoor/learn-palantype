{-# LANGUAGE TemplateHaskell #-}
module Github where

import Reflex.Dom (XhrResponse(..), TriggerEvent, PerformEvent, Performable, XhrRequest(..), xhrRequestConfig_headers, performRequestAsync, Prerender, Reflex(Event), XhrResponseBody(..))
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Data.Functor ((<&>))
import Data.Default (Default(def))
import Control.Lens ((&), (.~))
import Language.Javascript.JSaddle (MonadJSM)
import Data.Aeson ((.:), withObject, FromJSON (parseJSON))
import Palantype.Common.TH (fromJust)
import qualified Data.Aeson as Aeson

data RequestConfig = RequestConfig
  { rqcLang :: Text
  , rqcWrittenLang :: Text
  , rqcPageName :: Text
  }

data GithubResponseBody = GithubResponseBody
  { ghubContent :: Text
  , ghubEncoding :: Text
  }

instance FromJSON GithubResponseBody where
  parseJSON = withObject "response" $ \r -> GithubResponseBody
    <$> r .: "content"
    <*> r .: "encoding"

data GithubAPIResponse
  = GithubAPISuccess Text
  | GithubAPIError   Word Text

requestGithub
  :: forall t (m :: * -> *)
  . ( Prerender t m
    , MonadJSM (Performable m)
    , Monad m
    , PerformEvent t m
    , TriggerEvent t m
    )
  => Event t RequestConfig
  -> m (Event t GithubAPIResponse)
requestGithub eReq = do
    eResp <- performRequestAsync $ eReq <&> \RequestConfig{..} ->
        let
            url = "https://api.github.com/repos/rubenmoor/learn-palantype/main/cms-content/"
               <> rqcLang <> "/"
               <> rqcWrittenLang <> "/"
               <> rqcPageName <> ".md"
            config = def & xhrRequestConfig_headers .~ Map.fromList
                [ ("Accept", "application/vnd.github.raw")
                -- TODO:
                --  -H "Authorization: Bearer <YOUR-TOKEN>"\
                , ("X-GitHub-Api-Version", "2022-11-28")
                ]
        in  XhrRequest "GET" url config
    pure $ eResp <&> \XhrResponse{..} -> case _xhrResponse_status of
      200 -> let mbody = do
                   XhrResponseBody_ArrayBuffer xhrBody <- _xhrResponse_response
                   Aeson.decodeStrict' xhrBody
             in  GithubAPISuccess $ ghubEncoding $ $fromJust mbody
      code -> GithubAPIError code _xhrResponse_statusText
