module Render (cfileSink) where

import           Control.Monad
import           Data.ByteString.Builder
import           Data.Complex
import           IQ
import           Pipes
import qualified Pipes.Prelude as P
import           System.IO

foldChunk :: (Monad m, Monoid a) => Int -> Pipe a a m ()
foldChunk n = forever $ go n mempty
  where go 0 x = yield x
        go n x = do
          x' <- await
          go (n - 1) $ x <> x'

renderSample :: IQ -> Builder
renderSample (real :+ imag) = floatLE real <> floatLE imag

cfileSink :: MonadIO m => Handle -> Consumer IQ m ()
cfileSink h = P.map renderSample
           >-> foldChunk 100
           >-> P.mapM_ (liftIO . hPutBuilder h)
{-# INLINE cfileSink #-}
