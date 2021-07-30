module ZeroStuff (zeroStuff) where

import IQ
import Data.Complex
import Control.Monad
import Pipes

zeroStuff :: (Monad m, Num a) => Int -> Pipe a a m ()
zeroStuff factor = forever $ await >>= yield >> replicateM_ (factor - 1) (yield 0)
{-# INLINE zeroStuff #-}
