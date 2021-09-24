{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Stage where

import           Data.Function (($))
import           Reflex.Dom    (DomBuilder, el, text)

stage1_0
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  )
  => m ()
stage1_0 = do
  el "h1" $ text "Stage 1"
  el "h2" $ text "The stenographic alphabet"
  el "span" $ text "Type the following steno letters in order, one after another:"
  el "code" $ text "SCPTH+MFRNLYOEAUI^NLCMFRPT+SH"
  el "span" $ text "Some letters occur twice, the first time for your left hannd \
                   \and the second time for your right hand."
