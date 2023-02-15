module Page.Stage9 where

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
    [ ( (PatReplC, 0)
      , ( el "p" $ text "TODO"
        , el "p" $ text "TODO"
        )
    )
    , ( (PatReplC, 2)
      , ( el "p" $ text "TODO"
        , el "p" $ text "TODO"
        )
      )
    , ( (PatBreakUpI, 0)
      , ( el "p" $ text "TODO"
        , el "p" $ text "TODO"
        )
      )
    , ( (PatBreakUpI, 4)
      , ( el "p" $ text "TODO"
        , el "p" $ text "TODO"
        )
      )
    ]
