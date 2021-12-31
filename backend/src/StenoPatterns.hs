{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StenoPatterns
  ( checkParse
  , icaseChar
  , syll
  , replChar
  ) where

import           Data.Generics.Product          ( field )
import Text.Megaparsec (runParserT, MonadParsec, ParsecT, some, eof)
import Data.Text (head, length, null, Text, uncons, stripPrefix)
import Control.Monad (void, unless, MonadPlus, (>>=), (>>), mzero, guard)
import Data.Function ((&), ($))
import Data.Maybe (Maybe(..))
import Control.Applicative (Applicative(pure))
import Palantype.Common.RawSteno (RawSteno (..))
import TextShow (TextShow(showt))
import Data.Void (Void)
import Control.Monad.State.Class (gets, put, MonadState)
import Text.Megaparsec.Char (char')
import Control.Category ((<<<), (>>>))
import Data.Eq (Eq((==)))
import Control.Monad.State (evalState, State)
import Data.Either (isRight)
import Data.Bool (not, (||), (&&), Bool)
import Data.Char (Char)
import Control.Applicative (Alternative((<|>)))
import qualified Data.Text.Encoding as Text
import Data.Text (toUpper)
import Control.Category (Category(id))
import GHC.Generics (Generic)
import Control.Lens ((%~))
import Data.Foldable (Foldable(foldl))
import Data.List ((!!), (++))
import Data.Functor ((<$>))
import Palantype.DE (lsPrimitives)
import Data.List (elem)
import qualified Data.Text as Text
import Data.List (last)
import Data.List (tail)
import Data.Text (isInfixOf)
import Data.Foldable (any)
import Data.Text (isPrefixOf)

{-
get a disjunct partition of all patterns defined in DE/primitives.json5
-}

{-
single letter replacements
  onset: p, t, k, z, v, c, q, ...?
  coda: w, v, b, g, t, z
  nucleus: ö, y
multiple letter replacements
  onset: sp, st, sk, pf, sch, ch, schm, schw, qu
  coda: ch, sch, nch, tsch, tz, tzt, ts, ng, nk, lm
    consonants digraphs: ll, nn, mm, ss, ff, pp, tt
r in the coda: er, ar, or, ur, ir, är, ür, ör
h in the coda: eh, ah, oh, uh, äh, üh, öh
hr in the coda: ehr, ahr, ohr, uhr, ihr, ier, ühr, öhr

ß ?
vowel digraphs
coda: g?
ie, ih

capitalization
punctuation, hyphenation?
special characters
commands
steno tricks (e.g. capitalize last word, retroactively write +)

engl.: ?
frnz.: ?
chemical elements
acronyms

even faster:
en, es, er
de, di, ge, gi, be, bi, ...
onset: schl, schn, schr
-}
data StatePatterns = StatePatterns
  { stSimple :: [(Char, RawSteno)]
  , stSingle0 :: [(Char, RawSteno)]
  , stDigraphCons  :: [(Text, RawSteno)]
  , stMultiSimple :: [(Text, RawSteno)] -- remaining
  , stCodaR :: [(Text, RawSteno)]
  , stCodaHIE :: [(Text, RawSteno)]
  , stCodaHR :: [(Text, RawSteno)]
  , stSZ :: [(Text, RawSteno)]
  , stG :: [(Text, RawSteno)]
  , stDigraphVowels :: [(Text, RawSteno)]
  } deriving (Generic)

-- TODO
-- ignore rules with / in raw completely
-- do not ignore patterns with | in bs, but don't show those in html
patterns =
  let
      acc st (bs, (g, raw)) =
        let
            strRaw = showt raw
            str = Text.decodeUtf8 bs
            str' = Text.unpack str
            ucSimpl = toUpper $ stripDiacritics str
        in
            case g of
              0 -> if
                | length str == 1 -> if ucSimpl == strRaw
                    then st & field @"stSimple" %~ ((head str, raw) :)
                    else st & field @"stSingle0" %~ ((head str, raw) :)
                | str `elem` ["bb", "ff", "ll", "mm", "nn", "pp", "rr", "ss", "tt"] ->
                    st & field @"stDigraphCons" %~ ((str, raw) :)
                | length str == 2 && str' !! 1 == 'r' ->
                        st & field @"stCodaR" %~ ((str, raw) :)
                | length str == 2 && (str' !! 1 == 'h' || str == "ie") ->
                        st & field @"stCodaHIE" %~ ((str, raw) :)
                | length str == 3 && tail str' == "hr" ->
                    st & field @"stCodaHR" %~ ((str, raw) :)
                | "ß" `isInfixOf` str && (not $ "|" `isInfixOf` str) ->
                    st & field @"stSZ" %~ ((str, raw) :)
                | "g" `isInfixOf` str && (not $ "|" `isInfixOf` str) ->
                    st & field @"stG" %~ ((str, raw) :)
                | any (`isPrefixOf` str) ["aa", "ee", "oo"] ->
                    st & field @"stDigraphVowels" %~ ((str, raw) :)


  in  foldl acc (StatePatterns [] [] [] [] [] [] [] [] [] []) $
        foldl (\ls (bs, lsGRaw) -> ls ++ ((bs,) <$> lsGRaw)) [] lsPrimitives

stripDiacritics :: Text -> Text
stripDiacritics = id

{-
parse the dictionary
-}

checkParse :: ParsecT Void Text (State RawSteno) () -> Text -> RawSteno -> Bool
checkParse parserChar str = isRight <<< evalState (runParserT parser "" str)
  where
    parser = some parserChar >> end >> pure ()

-- TODO: replChar for accents
icaseChar
  :: ( MonadParsec Void Text m
     , MonadState RawSteno m
     )
  => m ()
icaseChar =
  gets (showt >>> uncons) >>= guardJust \(x, xs) -> do
    _ <- char' x
    put $ RawSteno xs

replChar
  :: ( MonadParsec Void Text m
     , MonadState RawSteno m
     )
  => Char
  -> Text
  -> m ()
replChar c r = do
    _ <- char' c
    mSuffix <- gets $ showt >>> stripPrefix r
    guardJust (put <<< RawSteno) mSuffix

syll
  :: ( MonadParsec Void Text m
     , MonadState RawSteno m
     )
  => m ()
syll = gets (showt >>> uncons) >>= guardJust \(x, xs) -> do
  guard (x == '/')
  put $ RawSteno xs

---

end
  :: ( MonadParsec Void Text m
     , MonadState RawSteno m
     )
  => m ()
end = do
    st <- gets showt
    guard $ null st
    eof

guardJust :: MonadPlus m => (a -> m b) -> Maybe a -> m b
guardJust _ Nothing = mzero
guardJust f (Just x) = f x
