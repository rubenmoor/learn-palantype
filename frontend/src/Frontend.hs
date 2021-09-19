{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Frontend where

import           Control.Monad               (void)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Language.Javascript.JSaddle (eval, liftJSM)

import           Obelisk.Configs             (HasConfigs (getConfig))
import           Obelisk.Frontend            (Frontend (..))
import           Obelisk.Generated.Static    (static)
import           Obelisk.Route               (R)

import           Reflex.Dom.Core             (blank, el, elAttr, prerender_,
                                              text, (=:))

import           Common.Api                  (commonStuff)
import           Common.Route                (FrontendRoute)


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "h1" $ text "Welcome to Obelisk!"
      el "p" $ text $ T.pack commonStuff

      -- `prerender` and `prerender_` let you choose a widget to run on the server
      -- during prerendering and a different widget to run on the client with
      -- JavaScript. The following will generate a `blank` widget on the server and
      -- print "Hello, World!" on the client.
      prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)

      elAttr "img" ("src" =: static @"obelisk.jpg") blank
      el "div" $ do
        exampleConfig <- getConfig "common/example"
        case exampleConfig of
          Nothing -> text "No config file found in config/common/example"
          Just s  -> text $ T.decodeUtf8 s
      return ()
  }
