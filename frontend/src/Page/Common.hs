{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Page.Common where

import           Common.Route           (FrontendRoute (..))
import           Control.Applicative    (Alternative ((<|>)),
                                         Applicative (pure, (*>), (<*)))
import           Control.Category       (Category (id, (.)))
import           Control.Lens           ((%~), (.~), (<&>))
import           Control.Monad          (Monad((>>), fail), when)
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.Bool              (Bool (..))
import           Data.Char              (Char)
import           Data.Either            (Either (..))
import           Data.Eq                (Eq ((==)))
import           Data.Function          (($))
import           Data.Functor           (void, ($>), (<$>))
import           Data.Generics.Product  (field)
import           Data.List              (drop, findIndex, (!!))
import           Data.Maybe             (Maybe (..))
import           Data.Ord               (Ord ((>)))
import           Data.Semigroup         (Semigroup((<>)), Endo (..))
import qualified Data.Set               as Set
import           Data.String            (String)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Tuple             (fst, snd)
import           Data.Witherable        (Filterable (filter))
import           GHC.Num                (Num ((+)))
import           Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl,
                                         SetRoute (setRoute), routeLink)
import           Reflex.Dom             (DomBuilder, EventName (Click),
                                         EventWriter, HasDomEvent (domEvent),
                                         MonadHold (holdDyn),
                                         PostBuild (getPostBuild), Prerender,
                                         Reflex (Event, never), blank, el,
                                         elClass, elClass', leftmost, text)
import           Shared                 (dynSimple, iFa, whenJust)
import           State                  (Message (..), Env (..), Navigation (..), Stage (..),
                                         State, stageUrl, updateState)
import           Text.Parsec            ((<?>), oneOf, space, spaces, Parsec, anyChar, char, eof, getState,
                                         many1, runParser, sepBy1, setState,
                                         try)
import qualified Text.Parsec            as Parsec
import           Text.Show              (Show (show))
import Data.Foldable (concat)
import Control.Monad (MonadPlus(mzero))
import Palantype.Common (Palantype, Chord)
import Palantype.Common.RawSteno (RawSteno, parseSteno)
import Data.Tagged (Tagged (..))
import Data.Proxy (Proxy(Proxy))
import TextShow (showt)

elFooter
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , Prerender js t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  )
  => Navigation
  -> m ()
elFooter Navigation{..} = el "footer" $ do
  whenJust navMPrevious $ \prv -> do
    elClass "div" "floatLeft" $ do
      text "< "
      routeLink (stageUrl prv) $ text $ Text.pack $ show prv
  text $ Text.pack $ show navCurrent
  whenJust navMNext $ \nxt -> do
    elClass "div" "floatRight" $ do
      routeLink (stageUrl nxt) $ text $ Text.pack $ show nxt
      text " >"
  elClass "br" "clearBoth" blank

elCongraz
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t (Endo State) m
  , MonadFix m
  , MonadHold t m
  , MonadReader (Env t) m
  , PostBuild t m
  , SetRoute t (R FrontendRoute) m
  )
  => Event t ()
  -> Navigation
  -> m ()
elCongraz eDone Navigation{..} = mdo

  eChord <- asks envEChord

  let eChordCON = void $ filter (== "GDON") eChord
      eChordBACK = void $ filter (== "BAK") eChord

  dynShowCongraz <- holdDyn False $ leftmost [eDone $> True, eBack $> False]
  eBack <- dynSimple $ dynShowCongraz <&> \case
    False -> pure never
    True ->
      elClass "div" "mkOverlay" $
        elClass "div" "congraz" $ do
          el "div" $ text "Task cleared!"
          el "div" $ iFa "fas fa-check-circle"
          whenJust navMNext $ \nxt -> do
            (elACont, _) <- elClass "div" "anthrazit" $ do
              text "Type "
              el "code" $ text "CON"
              text " to continue to "
              elClass' "a" "normalLink" (text $ Text.pack $ show nxt)
            let eContinue = leftmost [eChordCON, domEvent Click elACont]
            updateState $ eContinue $>
              [ field @"stProgress" %~ \s -> if nxt > s then nxt else s
              , field @"stCleared" %~ Set.insert navCurrent
              , if nxt == Stage2_1
                  then field @"stTOCShowStage2" .~ True
                  else id
              ]
            setRoute $ eContinue $> FrontendRoute_Main :/ ()
          el "div" $ do
            el "span" $ text "("
            (elABack, _) <- elClass' "a" "normalLink" $ text "back"
            text " "
            el "code" $ text "P+AC"
            el "span" $ text ")"
            pure $ leftmost [eChordBACK, domEvent Click elABack]
  blank

-- parserChord :: Parsec String ([(Char, PTChar)], Bool) PTChord
-- parserChord = do
--
--   let lsChars =
--         [ ('S', LeftS)
--         , ('C', LeftC)
--         , ('P', LeftP)
--         , ('T', LeftT)
--         , ('H', LeftH)
--         , ('+', LeftCross)
--         , ('M', LeftM)
--         , ('F', LeftF)
--         , ('R', LeftR)
--         , ('N', LeftN)
--         , ('L', LeftL)
--         , ('Y', LeftY)
--         , ('O', LeftO)
--         , ('E', LeftE)
--         , ('|', LeftPipe)
--         , ('|', RightPipe)
--         , ('A', RightA)
--         , ('U', RightU)
--         , ('I', MiddleI)
--         , ('^', RightPoint)
--         , ('N', RightN)
--         , ('L', RightL)
--         , ('C', RightC)
--         , ('M', RightM)
--         , ('F', RightF)
--         , ('R', RightR)
--         , ('P', RightP)
--         , ('T', RightT)
--         , ('+', RightCross)
--         , ('S', RightS)
--         , ('H', RightH)
--         , ('e', RightE)
--         ]
--   setState (lsChars, False)
--
--   let parserKey = do
--         (ls, _) <- getState
--         c <- oneOf $ fst <$> lsChars
--         case findIndex ((==) c . fst) ls of
--             Nothing -> mzero
--             Just i  -> do
--               Parsec.updateState $ (drop (i + 1) ls,) . snd
--               pure $ snd $ ls !! i
--
--       parserHypen = do
--         (ls, foundHyphen) <- getState
--         when foundHyphen mzero
--         void $ char '-'
--         setState (drop 15 ls, True)
--
--   PTChord <$> many1
--     (   parserKey
--     <|> (parserHypen *> parserKey)
--     )
--
-- parserWord :: Parsec String ([(Char, PTChar)], Bool) [PTChord]
-- parserWord = sepBy1 parserChord (char '/')
--
-- parserSentence :: Parsec String ([(Char, PTChar)], Bool) [[PTChord]]
-- parserSentence = spaces >> sepBy1 parserWord (many1 space) <* eof
--
-- parseSteno :: String -> Either Text [PTChord]
-- parseSteno str =
--   case runParser parserSentence ([], False) "frontend steno code" str of
--     Left  err -> Left  $ Text.pack $ show err
--     Right ls  -> Right $ concat ls

parseStenoOrError
  :: forall proxy key t (m :: * -> *).
  ( EventWriter t (Endo State) m
  , Palantype key
  , PostBuild t m
  )
  => proxy key
  -> RawSteno
  -> m (Maybe [Chord key])
parseStenoOrError _ raw =
  case parseSteno raw of
    Right words -> pure $ Just words
    Left  err   -> do
      ePb <- getPostBuild
      let msgCaption = "Internal error"
          msgBody    = "Could not parse steno code: " <> showt raw
                    <> "\n" <> err
      updateState $ ePb $> [field @"stMsg" .~ Just Message{..}]
      pure Nothing
