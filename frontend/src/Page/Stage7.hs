module Page.Stage7 where

import           Page.Common.Exercise           ( Constraints )
import           Palantype.Common               ( Greediness )
import           Palantype.DE                   ( Pattern(..) )
import           Reflex.Dom                     ( el
                                                , text
                                                )
import           Data.Map.Strict                ( Map )
import qualified Palantype.DE                  as DE
import qualified Data.Map.Strict               as Map

exercises
    :: forall key t (m :: * -> *)
     . Constraints key t m
    => Map (DE.Pattern, Greediness) (m (), m ())
exercises = Map.fromList
    [ ( (PatDt, 0)
      , ( el "p" $ text "TODO"
        , el "p" $ text "TODO"
        )
    )
    , ( (PatDt, 2)
      , ( el "p" $ text "TODO"
        , el "p" $ text "TODO"
        )
      )
    ]
