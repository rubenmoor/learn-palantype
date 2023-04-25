{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler
    ( handlers
    )
where

import           Servant.API                    ( (:<|>)(..) )
import           Servant.Server                 ( HasServer(ServerT) )

import           Common.Api                     ( RoutesApi )
import qualified Handler.Palantype             as Palantype
import qualified Handler.Auth                  as Auth
import qualified Handler.User                  as User
import qualified Handler.Event                 as Event
import qualified Handler.Admin                 as Admin
import qualified Handler.Stats                 as Stats
import qualified Handler.CMS                   as CMS
import           AppData                        ( Handler )
import Data.Text (Text)
import Control.Applicative (Applicative(..))
import Obelisk.Generated.Static (staticFileContent)
import qualified Data.Text.Encoding as Text
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char (Char)
import qualified Data.Text as Text
import Data.Bool (Bool)
import Data.Function (($))
import Data.List (filter, take)
import Data.Foldable (all)
import Data.String (String)
import Data.Int (Int)
import Data.Ord ((>))

handlers :: ServerT RoutesApi a Handler
handlers =
        Palantype.handlers
   :<|> Admin.handlers
   :<|> Auth.handlers
   :<|> User.handlers
   :<|> Event.handlers
   :<|> Stats.handlers
   :<|> CMS.handlers
   :<|> handleWordList

handleWordList :: String -> Int -> Handler Text
handleWordList letters max = do
    let sorted = Text.lines $ Text.decodeUtf8 $(staticFileContent "german.utf8.dic.sorted")
        ls = filter everyCharInSet sorted
        maxed = if max > 0 then take max ls else ls
    pure $ Text.unlines maxed
  where
    everyCharInSet :: Text -> Bool
    everyCharInSet str = all (`Set.member` Set.fromList letters) $ Text.unpack str
