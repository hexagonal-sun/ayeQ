module Render (renderSample) where

import Data.ByteString.Builder
import IQ
import Data.Complex

renderSample :: IQ -> Builder
renderSample (real :+ _) = floatLE real <> floatLE real
{-# INLINE renderSample #-}
