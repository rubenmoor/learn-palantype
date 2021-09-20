{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Client where

import           Common.Api          (PloverCfg, RoutesApi)
import           Control.Applicative (Applicative (pure))
import           Control.Monad       (Monad)
import           Data.Either         (Either)
import           Data.Functor        ((<$>))
import           Data.Proxy          (Proxy (Proxy))
import           Data.Text           (Text)
import           Reflex.Dom          (Prerender (Client, prerender),
                                      Reflex (Dynamic, Event, never), constDyn,
                                      switchDyn)
import           Servant.Common.Req  (ReqResult)
import           Servant.Reflex      (BaseUrl (BasePath), SupportsServantReflex,
                                      client)
import Data.String (String)

postRender
  :: (Prerender js t m, Monad m)
  => Client m (Event t a)
  -> m (Event t a)
postRender action = switchDyn <$> prerender (pure never) action

postConfigNew
  :: SupportsServantReflex t m
  => Dynamic t (Either Text String)
  -> Event t ()
  -> m (Event t (ReqResult () PloverCfg))

postConfigNew =
  client (Proxy :: Proxy RoutesApi)
         (Proxy :: Proxy (m :: * -> *))
         (Proxy :: Proxy ())
         (constDyn (BasePath "/"))
