module PageBlog
  ( pageBlog
  ) where

import           Control.Monad.Fix              ( MonadFix )
import           Reflex.Dom                     ( DomBuilder(..)
                                                , MonadHold(..)
                                                , PostBuild(..)
                                                , Prerender
                                                , blank
                                                )
import State (Env(..))
import Control.Monad.Reader (MonadReader(ask))

pageBlog
  :: forall t (m :: * -> *)
  . ( DomBuilder t m
    , PostBuild t m
    , Prerender t m
    , MonadFix m
    , MonadHold t m
    )
  => m ()
pageBlog = do
    -- Env {..} <- ask
    -- evLoadedAndBuilt <- envGetLoadedAndBuilt
    blank
