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

import           Common.Api          (PloverSystemCfg, RoutesApi)
import           Control.Applicative (Applicative (pure))
import           Control.Monad       (Monad)
import           Data.Either         (Either(Right))
import           Data.Functor        ((<$>))
import           Data.Proxy          (Proxy (Proxy))
import           Data.String         (String)
import           Data.Text           (Text)
import           Data.Witherable     (Filterable (mapMaybe))
import           Reflex.Dom          (Prerender (Client, prerender),
                                      Reflex (Dynamic, Event, never), constDyn,
                                      switchDyn, XhrResponse (..), )
import           Servant.Common.Req  (ReqResult(..),  reqSuccess)
import           Servant.Reflex      (BaseUrl (BasePath), SupportsServantReflex,
                                      client)
import Data.Maybe (fromMaybe, Maybe (..))
import Data.Function (($))
import Data.Semigroup (Semigroup((<>)))
import Servant.API ((:<|>)(..))
import Palantype.Common.RawSteno (RawSteno)
import Palantype.Common (Lang)
import Data.Int (Int)
import Data.Map.Strict (Map)
import qualified Palantype.DE.Pattern as DE
import Palantype.DE (Greediness)

postRender
  :: (Prerender js t m, Monad m)
  => Client m (Event t a)
  -> m (Event t a)
postRender action = switchDyn <$> prerender (pure never) action

data RequestResult t a = RequestResult
  { rrESuccess :: Event t a
  , rrEFailure :: Event t Text
  }

request
  :: forall js t (m :: * -> *) a.
  ( Monad m
  , Prerender js t m
  )
  => Client m (Event t (ReqResult () a))
  -> m (RequestResult t a)
request req = do
  eResult <- switchDyn <$> prerender (pure never) req
  let rrESuccess = mapMaybe reqSuccess eResult
      rrEFailure = mapMaybe reqFailure eResult
  pure RequestResult{..}

postConfigNew
  :: SupportsServantReflex t m
  => Dynamic t (Either Text String)
  -> Event t ()
  -> m (Event t (ReqResult () (Lang, PloverSystemCfg)))

getDictDE
  :: SupportsServantReflex t m
  => Dynamic t (Either Text Int)
  -> Dynamic t (Either Text DE.Pattern)
  -> Dynamic t (Either Text Greediness)
  -> Event t ()
  -> m (Event t (ReqResult () (Map RawSteno Text, Map Text [RawSteno])))

getDictDE'
  :: SupportsServantReflex t m
  => Int
  -> DE.Pattern
  -> Greediness
  -> Event t ()
  -> m (Event t (ReqResult () (Map RawSteno Text, Map Text [RawSteno])))
getDictDE' n p g =
  getDictDE (constDyn $ Right n) (constDyn $ Right p) (constDyn $ Right g)

postConfigNew :<|> getDictDE =
  client (Proxy :: Proxy RoutesApi)
         (Proxy :: Proxy (m :: * -> *))
         (Proxy :: Proxy ())
         (constDyn (BasePath "/"))

reqFailure :: ReqResult tag a -> Maybe Text
reqFailure = \case
  ResponseSuccess {}        -> Nothing
  ResponseFailure _ str xhr -> Just $ str <> fromMaybe "" (_xhrResponse_responseText xhr)
  RequestFailure  _ str     -> Just str

--     :<|> "dict"   :> Capture "n" Int :>
--         (  "singlesimple" :> Get '[JSON] BDict
--       :<|> "multiplesimple" :> Get '[JSON] BDict
--       :<|> "singleletterreplacements" :> Get '[JSON] BDict
--       :<|> "multipleletterreplacements" :> Get '[JSON] BDict
--       :<|> "codahr" :> Get '[JSON] BDict
--         )
