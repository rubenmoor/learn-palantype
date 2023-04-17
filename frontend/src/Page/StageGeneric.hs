{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}

module Page.StageGeneric
    ( getGenericExercise
    ) where

import           CMS                            ( elCMS, elCMSContent )
import           Common.Route                   ( FrontendRoute )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader           ( ask )
import           Control.Monad.Reader.Class     ( MonadReader )
import           Data.Functor                   ( (<&>) )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( isNothing )
import           Data.Semigroup                 ( Endo )
import           Obelisk.Route.Frontend         ( R
                                                , SetRoute
                                                )
import           Page.Common                    ( elCongraz
                                                , elPatterns
                                                , getStatsLocalAndRemote
                                                , taskWords
                                                )
import           Palantype.Common               ( Greediness
                                                , Palantype
                                                , patternDoc
                                                )
import qualified Palantype.DE                  as DE
import           PloverDict                     ( getMapsForExercise )
import           Reflex.Dom                     ( DomBuilder
                                                , EventWriter
                                                , MonadHold
                                                , PerformEvent(..)
                                                , PostBuild
                                                , Prerender
                                                , Reflex(..)
                                                , TriggerEvent
                                                , blank
                                                , elClass
                                                , gate
                                                , splitE
                                                , text
                                                )
import           State                          ( Env(..)
                                                , State
                                                )
import           Witherable                     ( Filterable(mapMaybe) )

getGenericExercise
  :: forall key t (m :: * -> *)
  . ( DomBuilder t m
    , EventWriter t (Endo State) m
    , MonadFix m
    , MonadHold t m
    , MonadReader (Env t key) m
    , Palantype key
    , MonadIO (Performable m)
    , PerformEvent t m
    , PostBuild t m
    , Prerender t m
    , SetRoute t (R FrontendRoute) m
    , TriggerEvent t m
    )
  => DE.Pattern
  -> Greediness
  -> m ()
getGenericExercise patternGroup greediness = mdo

    Env{..} <- ask

    (evPart1, evPart2) <- elCMS 2 <&> splitE . mapMaybe \case
      [p1, p2] -> Just (p1, p2)
      _        -> Nothing

    elCMSContent evPart1

    elPatterns
        $ Map.toList
        $ Map.findWithDefault Map.empty greediness
        $ Map.findWithDefault Map.empty patternGroup patternDoc

    elCMSContent evPart2

    dynStatsAll <- getStatsLocalAndRemote evDone

    evDone <- case getMapsForExercise patternGroup greediness of
        Left str -> do
            elClass "p" "text-sm text-red-500" $ text $ "Couldn't load exercise: " <> str
            pure never
        Right (mSW, mWSs) -> taskWords
            dynStatsAll
            (gate (not <$> current dynDone) envEvMChord)
            mSW
            mWSs

    let dynStatsPersonal = fmap snd . filter (isNothing . fst) . fmap snd <$> dynStatsAll
    dynDone <- elCongraz (Just <$> evDone) dynStatsPersonal envNavigation

    blank
