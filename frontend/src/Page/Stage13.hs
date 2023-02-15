module Page.Stage13 where

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
    [ ( (PatReplRare, 0)
      , ( el "p" $ text "TODO"
        , el "p" $ text "TODO"
        )
    )
    , ( (PatReplRare, 3)
      , ( el "p" $ text "TODO"
        , el "p" $ text "TODO"
        )
      )
    , ( (PatSmallS, 0)
      , ( el "p" $ text "TODO"
        , el "p" $ text "TODO"
        )
      )
    , ( (PatSmallS, 6)
      , ( el "p" $ text "TODO"
        , el "p" $ text "TODO"
        )
      )
    ]
