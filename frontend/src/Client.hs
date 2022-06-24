{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Client where

import           Common.PloverConfig            ( PloverSystemCfg )
import           Common.Api                     ( RoutesApi )
import           Common.Auth                    ( CompactJWT
                                                , SessionData(..)
                                                , LoginData(..)
                                                , UserNew(..)
                                                )
import           Control.Applicative            ( Applicative(pure) )
import           Control.Monad                  ( Monad )
import           Data.Either                    ( either
                                                , Either(..)
                                                )
import           Data.Functor                   ( (<&>)
                                                , (<$>)
                                                )
import           Data.Proxy                     ( Proxy(Proxy) )
import           Data.String                    ( String )
import           Data.Text                      ( Text )
import           Reflex.Dom                     ( XhrResponseBody(..)
                                                , xhrResponse_response
                                                , Prerender(Client, prerender)
                                                , Reflex(Dynamic, Event, never)
                                                , constDyn
                                                , switchDyn
                                                , XhrResponse(..)
                                                )
import           Servant.Common.Req             ( ReqResult(..) )
import           Servant.Reflex                 ( BaseUrl(BasePath)
                                                , SupportsServantReflex
                                                , client
                                                )
import           Data.Maybe                     ( fromMaybe
                                                , Maybe(..)
                                                )
import           Data.Function                  ( const
                                                , ($)
                                                )
import           Data.Semigroup                 ( Semigroup((<>)) )
import           Servant.API                    ( (:<|>)(..) )
import           Palantype.Common               ( MapStenoWordTake100
                                                , PatternDoc
                                                , PatternPos
                                                , RawSteno
                                                )
import           Palantype.Common               ( Greediness
                                                , Lang
                                                )
import           Data.Map.Strict                ( Map )
import qualified Palantype.DE                  as DE
import           Data.Functor                   ( Functor )
import           State                          ( stSession
                                                , State
                                                , Session(..)
                                                )
import           Data.Bool                      ( Bool )
import           Control.Lens.Getter            ( (^.) )
import           Data.Text.Encoding             ( decodeUtf8' )
import Common.Model (AppState)

postRender :: (Prerender t m, Monad m) => Client m (Event t a) -> m (Event t a)
postRender action = switchDyn <$> prerender (pure never) action

-- data RequestResult t a = RequestResult
--   { rrESuccess :: Event t a
--   , rrEFailure :: Event t Text
--   }

request
    :: forall t (m :: * -> *) a
     . (Monad m, Prerender t m)
    => Client m (Event t (ReqResult () a))
    -> m (Event t (Either Text a))
request req = do
    eResult <- switchDyn <$> prerender (pure never) req
    pure $ eResult <&> \case
        ResponseSuccess _ x _ -> Right x
        ResponseFailure _ _ resp ->
          let mErrBody = do
                  responseBody <- resp ^. xhrResponse_response
                  bs           <- case responseBody of
                      XhrResponseBody_ArrayBuffer bs -> pure bs
                      _                              -> Nothing
                  either (const Nothing) pure $ decodeUtf8' bs
          in  Left $ fromMaybe "Couldn't decode error body" mErrBody
        RequestFailure _ str -> Left $ "RequestFailure: " <> str

  -- let rrESuccess = mapMaybe reqSuccess eResult
  --     rrEFailure = mapMaybe reqFailure eResult
  -- pure RequestResult{..}

postConfigNew
    :: SupportsServantReflex t m
    => Dynamic t (Either Text String)
    -> Event t ()
    -> m (Event t (ReqResult () (Lang, PloverSystemCfg)))

getDocDEPatternAll
    :: SupportsServantReflex t m
    => Event t ()
    -> m
           ( Event
                 t
                 ( ReqResult
                       ()
                       (PatternDoc DE.Key, MapStenoWordTake100 DE.Key)
                 )
           )

getDocDEPattern
    :: SupportsServantReflex t m
    => Dynamic t (Either Text DE.Pattern)
    -> Dynamic t (Either Text Greediness)
    -> Event t ()
    -> m (Event t (ReqResult () [(PatternPos, [(Text, RawSteno)])]))

getDocDEPattern'
    :: SupportsServantReflex t m
    => DE.Pattern
    -> Greediness
    -> Event t ()
    -> m (Event t (ReqResult () [(PatternPos, [(Text, RawSteno)])]))

getDocDEPattern' p g =
    getDocDEPattern (constDyn $ Right p) (constDyn $ Right g)

getDictDE
    :: SupportsServantReflex t m
    => Dynamic t (Either Text DE.Pattern)
    -> Dynamic t (Either Text Greediness)
    -> Event t ()
    -> m
           ( Event
                 t
                 ( ReqResult
                       ()
                       (Map RawSteno Text, Map Text [RawSteno])
                 )
           )

getDictDE'
    :: SupportsServantReflex t m
    => DE.Pattern
    -> Greediness
    -> Event t ()
    -> m
           ( Event
                 t
                 ( ReqResult
                       ()
                       (Map RawSteno Text, Map Text [RawSteno])
                 )
           )
getDictDE' p g = getDictDE (constDyn $ Right p) (constDyn $ Right g)

getDictDENumbers
    :: SupportsServantReflex t m
    => Event t ()
    -> m (Event t (ReqResult () (Map RawSteno Text)))

-- auth

getAuthData
    :: Functor (Dynamic t)
    => Dynamic t State
    -> Dynamic t (Either Text (CompactJWT, Text))
getAuthData dynState = dynState <&> \st -> case stSession st of
    SessionAnon                  -> Left "not logged in"
    SessionUser SessionData {..} -> Right (sdJwt, sdAliasName)

postAuthenticate
    :: SupportsServantReflex t m
    => Dynamic t (Either Text LoginData)
    -> Event t ()
    -> m (Event t (ReqResult () (Maybe (SessionData, AppState))))

postAuthNew
    :: SupportsServantReflex t m
    => Dynamic t (Either Text UserNew)
    -> Event t ()
    -> m (Event t (ReqResult () SessionData))

postDoesUserExist
    :: SupportsServantReflex t m
    => Dynamic t (Either Text Text)
    -> Event t ()
    -> m (Event t (ReqResult () Bool))

-- user

postAliasRename
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (CompactJWT, Text))
    -> Dynamic t (Either Text Text)
    -> Event t ()
    -> m (Event t (ReqResult () ()))

getAliasAll
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (CompactJWT, Text))
    -> Event t ()
    -> m (Event t (ReqResult () [Text]))

postAliasSetDefault
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (CompactJWT, Text))
    -> Dynamic t (Either Text Text)
    -> Event t ()
    -> m (Event t (ReqResult () ()))

getAppState
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (CompactJWT, Text))
    -> Event t ()
    -> m (Event t (ReqResult () AppState))

postAppState
    :: SupportsServantReflex t m
    => Dynamic t (Either Text (CompactJWT, Text))
    -> Dynamic t (Either Text AppState)
    -> Event t ()
    -> m (Event t (ReqResult () ()))

((postConfigNew :<|> getDocDEPatternAll :<|> getDocDEPattern :<|> getDictDE :<|> getDictDENumbers) :<|> (postAuthenticate :<|> postAuthNew :<|> postDoesUserExist) :<|> ((postAliasRename :<|> getAliasAll :<|> postAliasSetDefault) :<|> (getAppState :<|> postAppState)))
    = client (Proxy :: Proxy RoutesApi)
             (Proxy :: Proxy (m :: * -> *))
             (Proxy :: Proxy ())
             (constDyn (BasePath "/"))

reqFailure :: ReqResult tag a -> Maybe Text
reqFailure = \case
    ResponseSuccess{} -> Nothing
    ResponseFailure _ str xhr ->
        Just $ str <> fromMaybe "" (_xhrResponse_responseText xhr)
    RequestFailure _ str -> Just str

--     :<|> "dict"   :> Capture "n" Int :>
--         (  "singlesimple" :> Get '[JSON] BDict
--       :<|> "multiplesimple" :> Get '[JSON] BDict
--       :<|> "singleletterreplacements" :> Get '[JSON] BDict
--       :<|> "multipleletterreplacements" :> Get '[JSON] BDict
--       :<|> "codahr" :> Get '[JSON] BDict
--         )
