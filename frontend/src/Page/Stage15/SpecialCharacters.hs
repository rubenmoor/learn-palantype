{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Page.Stage15.SpecialCharacters
    ( specialCharacters
    ) where

import Control.Monad.Reader.Class (MonadReader, ask)
import Reflex.Dom (DomBuilder, EventWriter, TriggerEvent, MonadHold, PerformEvent (..), PostBuild, Prerender, el, text, blank, Reflex (..), gate, foldDyn, holdUniqDyn, elClass, dyn_, splitE)
import State (Env (..), State)
import CMS (elCMSContent, elCMS)
import Witherable (Filterable(mapMaybe, filter))
import Data.Functor ((<&>), (<$>), Functor (fmap))
import Data.Maybe (Maybe(..), isNothing)
import Data.Semigroup (Endo, Semigroup ((<>)))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Data.Function (($), (&), (.))
import Data.Foldable (Foldable(length), for_)
import Data.String (String)
import Data.Int (Int)
import Palantype.Common (Palantype, Chord, kiBackUp)
import Data.List ((!!), zip)
import Data.Eq (Eq((==)))
import GHC.Generics (Generic)
import Data.Generics.Sum              ( _As )
import Common.Model (Stats)
import qualified Palantype.Common.Indices as KI
import qualified Palantype.Common.RawSteno as Raw
import qualified Data.Text as Text
import Page.Common (getStatsLocalAndRemote, chordStart, elCongraz)
import Data.Bool (Bool, not)
import Data.Text (Text)
import Data.Tuple (fst, snd, swap)
import Palantype.Common.Dictionary (dictLiterals)
import Data.Generics.Product (HasField(..))
import Control.Lens ((.~), (+~), (?~))
import GHC.Num (Num((-)))
import Page.Common.Stopwatch (mkStopwatch, elStopwatch)
import TextShow (TextShow(showt))
import Data.Ord (Ord((>)))
import Shared (whenJust)
import Obelisk.Route.Frontend (SetRoute)
import Obelisk.Route (R)
import Common.Route (FrontendRoute)
import qualified Data.Map.Strict as Map
import Palantype.Common.TH (fromJust)

specialCharacters
  :: forall key t (m :: * -> *)
  . ( DomBuilder t m
    , EventWriter t (Endo State) m
    , MonadFix m
    , MonadHold t m
    , MonadIO (Performable m)
    , MonadReader (Env t key) m
    , Palantype key
    , PerformEvent t m
    , PostBuild t m
    , Prerender t m
    , SetRoute t (R FrontendRoute) m
    , TriggerEvent t m
    )
  => m ()
specialCharacters = mdo
    Env {..} <- ask

    (evPart1, evPart2) <- elCMS 2 <&> splitE . mapMaybe \case
      [c1, c2] -> Just (c1, c2)
      _   -> Nothing

    elCMSContent evPart1

    let listing =
          "handleWordList :: Text -> Int -> Bool -> Handler [Text]\n\
          \handleWordList letters max bCaseInsensitive = do\n\
          \\tlet sorted = Text.lines $ Text.decodeUtf8 $(staticFileContent \"german.utf8.dic.sorted\")\n\
          \\t\tls = filter everyCharInSet sorted\n\
          \\tpure if max > 0 then take max ls else ls\n\
          \  where\n\
          \\teveryCharInSet :: Text -> Bool\n\
          \\teveryCharInSet str =\n\
          \\t\tlet strCase = if bCaseInsensitive\n\
          \\t\t\t\t\t\tthen Text.toLower str\n\
          \\t\t\t\t\t\telse str\n\
          \\t\tin  all (`Set.member` Set.fromList lettersCase) $ Text.unpack strCase\n\
          \\n\
          \\tlettersCase = Text.unpack $ URI.decodeText $\n\
          \\t\tif bCaseInsensitive\n\
          \\t\tthen Text.toLower letters\n\
          \\t\telse letters\n"

    dynStatsAll <- getStatsLocalAndRemote evDone
    evDone <- taskListing listing dynStatsAll $ gate (not <$> current dynDone) envEvMChord
    let dynStatsPersonal = fmap snd . filter (isNothing . fst) . fmap snd <$> dynStatsAll
    dynDone <- elCongraz (Just <$> evDone) dynStatsPersonal envNavigation

    elCMSContent evPart2

data StateSpecial k
  = StatePause Int
  | StateRun (Run k)
  deriving Generic

data Run k = Run
    { stCounter  :: Int
    , stMMistake :: Maybe (Int, Chord k)
    , stNMistakes :: Int
    } deriving Generic

taskListing
    :: forall key t (m :: * -> *)
     . ( DomBuilder t m
       , EventWriter t (Endo State) m
       , MonadFix m
       , MonadHold t m
       , MonadIO (Performable m)
       , MonadReader (Env t key) m
       , Palantype key
       , PerformEvent t m
       , PostBuild t m
       , Prerender t m
       , TriggerEvent t m
       )
    => String
    -> Dynamic t [(Bool, (Maybe Text, Stats))]
    -> Event t (Maybe (Chord key))
    -> m (Event t Stats)
taskListing listing dynStats evMChord = do
  let
      len = length listing

      map = Map.fromList $ swap <$> dictLiterals

      step :: Maybe (Chord key) -> StateSpecial key -> StateSpecial key
      step (Just c) (StatePause _) | c == chordStart = stepStart
      step _        st@(StatePause _) = st
      step mc       st@(StateRun Run {..}) =

          let currentLiteral = KI.toRaw @key $ $fromJust $ Map.lookup (listing !! stCounter) map
          in  case (stMMistake, mc) of

                -- mistake mode ...
                -- ... back up
                (_, Nothing) ->
                    st & _As @"StateRun" . field @"stMMistake" .~ Nothing
                -- ... or do nothing
                (Just _, Just _) -> st

                -- correct
                (Nothing, Just c) | Raw.fromChord c == currentLiteral ->
                  if stCounter == len - 1
                      then StatePause stNMistakes
                      else st & _As @"StateRun" . field @"stCounter" +~ 1

                -- mistake
                (Nothing, Just c) ->
                    st &  _As @"StateRun" .  field @"stMMistake" ?~ (stCounter, c)
                       &  _As @"StateRun" .  field @"stNMistakes" +~ 1

      stepStart = StateRun Run
        { stCounter  = 0
        , stMMistake = Nothing
        , stNMistakes = 0
        }
      stateInitial = StatePause 0

  dynListing <- foldDyn step stateInitial evMChord

  evStartStop <- fmap updated $ holdUniqDyn $ dynListing <&> \case
      StatePause nMistakes -> nMistakes
      StateRun   _         -> -1

  dynStopwatch <- mkStopwatch evStartStop

  elClass "div" "mt-8 text-lg" do
      dyn_ $ dynListing <&> \st -> do
          elClass "pre" "bg-zinc-200 rounded p-3 block text-sm text-zinc-500"
            $ for_ (zip [0 :: Int ..] listing) \(i, lit) -> do
              let
                  clsBase = if lit == '\n' then "p-1" else ""
                  clsBg = case st of
                    StatePause _ -> "text-black"
                    StateRun Run{..} | stCounter > i -> "text-black"
                    StateRun Run{..} -> case stMMistake of
                      Just (j, _) -> if i == j         then "border border-red-500 -mx-[1px]"   else ""
                      Nothing     -> if stCounter == i then "text-zinc-200 bg-grayishblue-900" else ""
              elClass "code" (Text.unwords [clsBase, clsBg]) $ text $ Text.singleton lit
          el "br" blank
          case st of
              StatePause _ -> el "div" do
                  text "Type "
                  elClass "span" "steno-action" do
                      text "Start "
                      el "code" $ text $ showt $ chordStart @key
                  text " to begin the exercise."
              StateRun Run {..} -> do
                  whenJust stMMistake $ \(_, w) -> do
                    elClass "p" "text-red-500 text-sm ml-1" do
                        text "You typed "
                        el "code" $ text $ showt w
                        elClass "span" "steno-navigation p-1 ml-2"
                            $  text
                            $  "↤ "
                            <> showt (KI.toRaw @key kiBackUp) -- U+21A4
                    el "br" blank

                  text $ showt stCounter <> " / " <> showt len

      elStopwatch dynStats dynStopwatch len
