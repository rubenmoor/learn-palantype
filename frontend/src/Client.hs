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
import           Data.Either         (Either(..))
import           Data.Functor        ((<&>), (<$>))
import           Data.Proxy          (Proxy (Proxy))
import           Data.String         (String)
import           Data.Text           (Text)
import           Reflex.Dom          (Prerender (Client, prerender),
                                      Reflex (Dynamic, Event, never), constDyn,
                                      switchDyn, XhrResponse (..), )
import           Servant.Common.Req  (ReqResult(..))
import           Servant.Reflex      (BaseUrl (BasePath), SupportsServantReflex,
                                      client)
import Data.Maybe (fromMaybe, Maybe (..))
import Data.Function (($))
import Data.Semigroup (Semigroup((<>)))
import Servant.API ((:<|>)(..))
import Palantype.Common (MapStenoWordTake100, PatternDoc, PatternPos, RawSteno)
import Palantype.Common (Greediness, Lang)
import Data.Map.Strict (Map)
import qualified Palantype.DE as DE

postRender
  :: (Prerender t m, Monad m)
  => Client m (Event t a)
  -> m (Event t a)
postRender action = switchDyn <$> prerender (pure never) action

-- data RequestResult t a = RequestResult
--   { rrESuccess :: Event t a
--   , rrEFailure :: Event t Text
--   }

request
  :: forall t (m :: * -> *) a.
  ( Monad m
  , Prerender t m
  )
  => Client m (Event t (ReqResult () a))
  -> m (Event t (Either Text a))
request req = do
  eResult <- switchDyn <$> prerender (pure never) req
  pure $ eResult <&> \case
    ResponseSuccess _ x   _ -> Right x
    ResponseFailure _ str _ -> Left str
    RequestFailure  _ str   -> Left str

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
  -> m (Event t (ReqResult () (PatternDoc DE.Key, MapStenoWordTake100 DE.Key)))

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
  -> m (Event t (ReqResult () (Map RawSteno Text, Map Text [RawSteno])))

getDictDE'
  :: SupportsServantReflex t m
  => DE.Pattern
  -> Greediness
  -> Event t ()
  -> m (Event t (ReqResult () (Map RawSteno Text, Map Text [RawSteno])))
getDictDE' p g =
  getDictDE (constDyn $ Right p) (constDyn $ Right g)

getDictDENumbers
  :: SupportsServantReflex t m
  => Event t ()
  -> m (Event t (ReqResult () (Map RawSteno Text)))

postConfigNew :<|> getDocDEPatternAll :<|> getDocDEPattern :<|> getDictDE :<|> getDictDENumbers =
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
