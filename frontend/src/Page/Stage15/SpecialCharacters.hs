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

module Page.Stage15.SpecialCharacters
    ( specialCharacters
    ) where

import Control.Monad.Reader.Class (MonadReader, ask)
import Reflex.Dom (DomBuilder, EventWriter, TriggerEvent, MonadHold, PerformEvent (..), PostBuild, Prerender, el, text, blank, Reflex (..), gate, foldDyn, holdUniqDyn, elClass, dyn_)
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
import Data.Tuple (fst, snd)
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

    evContent <- elCMS 1 <&> mapMaybe \case
      [c] -> Just c
      _   -> Nothing

    elCMSContent evContent

    dynStatsAll <- getStatsLocalAndRemote evDone
    evDone <- taskListing dynStatsAll $ gate (not <$> current dynDone) envEvMChord
    let dynStatsPersonal = fmap snd . filter (isNothing . fst) . fmap snd <$> dynStatsAll
    dynDone <- elCongraz (Just <$> evDone) dynStatsPersonal envNavigation

    blank

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
    => Dynamic t [(Bool, (Maybe Text, Stats))]
    -> Event t (Maybe (Chord key))
    -> m (Event t Stats)
taskListing dynStats evMChord = do
  let
      listing :: String
      listing =
        "handleWordList :: Text -> Int -> Bool -> Handler [Text]\n\
        \handleWordList letters max bCaseInsensitive = do\n\
        \    let sorted = Text.lines $ Text.decodeUtf8 $(staticFileContent \"german.utf8.dic.sorted\")\n\
        \        ls = filter everyCharInSet sorted\n\
        \    pure if max > 0 then take max ls else ls\n\
        \  where\n\
        \    everyCharInSet :: Text -> Bool\n\
        \    everyCharInSet str =\n\
        \      let strCase = if bCaseInsensitive\n\
        \                    then Text.toLower str\n\
        \                    else str\n\
        \      in  all (`Set.member` Set.fromList lettersCase) $ Text.unpack strCase\n\
        \n\
        \    lettersCase = Text.unpack $ URI.decodeText $\n\
        \      if bCaseInsensitive\n\
        \      then Text.toLower letters\n\
        \      else letters\n"
      len = length listing

      map = Map.fromList

      step :: Maybe (Chord key) -> StateSpecial key -> StateSpecial key
      step (Just c) (StatePause _) | c == chordStart = stepStart
      step _        st@(StatePause _) = st
      step mc       st@(StateRun Run {..}) =

          let currentLiteral = KI.toRaw @key $ fst $ listing !! stCounter
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
      dyn_ $ dynListing <&> \case
          StatePause _ -> el "div" do
              text "Type "
              elClass "span" "steno-action" do
                  text "Start "
                  el "code" $ text $ showt $ chordStart @key
              text " to begin the exercise."
          StateRun Run {..} -> do
              elClass "div" "bg-zinc-200 rounded p-1 block"
                $ for_ (zip [0 :: Int ..] listing) \(i, lit) -> do
                  let
                      clsBase = "p-1"
                      clsBg = case stMMistake of
                          Just (j, _) -> if i == j        then "bg-red-500"   else ""
                          Nothing     -> if stCounter > i then "bg-green-500" else ""
                  elClass "code" (Text.unwords [clsBase, clsBg]) $ text $ Text.singleton lit

              el "br" blank
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
