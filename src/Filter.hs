module Filter (FilterOptions(..), parseOpts, lowPass, convolve) where

import           Control.Monad.Primitive
import           Control.Monad.State.Strict
import           Data.Coerce
import           Data.Complex
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import           Foreign.C.Types
import           Foreign.Ptr
import           IQ
import           Options.Applicative
import           Pipes

data FilterOptions = FilterOptions
  { cutoff :: Int
  , transitionWidth :: Int }

parseOpts :: Parser FilterOptions
parseOpts = FilterOptions
         <$> option auto
             ( long "cutoff-frequency"
            <> short 'c'
            <> value 15000
            <> showDefault
            <> metavar "CUTOFF_FREQ"
            <> help "Cutoff frequency, value in Hz")
         <*> option auto
             ( long "transition-width"
            <> short 't'
            <> value 10000
            <> showDefault
            <> metavar "TRANS_WIDTH"
            <> help "Transition width, value in Hz")

calculateNumTaps :: Int -> Int -> Int
calculateNumTaps sr tw = round $ 70.0 * sr' / (22.0 * tw')
  where sr' = fromIntegral sr
        tw' = fromIntegral tw

hann :: Floating a => Int -> [a]
hann n = map (\x -> sin (pi * (fromIntegral x / n')) ** 2) vec
  where vec = [0..n-1]
        n' =  fromIntegral n

-- sin (pi * x) / x. The cutoff frequency controls the sampling frequency of ths
-- sinc function: Sf = 2 * Fc, where Fc is expressed as a fraction of the
-- original signal sampling rate.
sinc :: Floating a =>  Int -> Int -> Int -> [a]
sinc len sr fc = map ((fac *) . sinc') [-(len `div` 2)..(len `div` 2)]
  where sinc' x | x == 0 = 1
                | otherwise =  let n = fromIntegral x in sin (fac * n * pi) / (fac * n * pi)
        fac = 2 * (fromIntegral fc / fromIntegral sr)

lowPass :: Int -> FilterOptions -> VS.Vector IQ
lowPass sr opts = VS.fromList x
  where nTaps = calculateNumTaps sr (transitionWidth opts)
        window = hann nTaps
        x = zipWith (*) window $ sinc nTaps sr (cutoff opts)

convolve :: VS.Vector IQ -> Pipe IQ IQ IO ()
convolve kernel = do
  let klen = VS.length kernel
      bufSz = 1024
      mkCInt = CInt . fromIntegral

  inBuf <- lift $ VSM.replicate bufSz 0.0
  outBuf <- lift $ VSM.replicate bufSz 0.0
  stateBuf <- lift $ VSM.replicate klen (0.0 :+ 0.0 :: IQ)

  forever $ do
    forM_ [0..bufSz-1] (\i -> do
      x <- await
      lift $ VSM.write inBuf i x)

    lift $ VS.unsafeWith (coerce kernel) $ \kBuf ->
      VSM.unsafeWith (coerce stateBuf) $ \stateBuf ->
        VSM.unsafeWith (coerce inBuf) $ \inBuf ->
          VSM.unsafeWith (coerce outBuf) $ \outBuf ->
            conv_c (mkCInt klen) (mkCInt bufSz) kBuf stateBuf inBuf outBuf

    forM_ [0..bufSz-1] (\i -> do
      nextSamp <- lift $ VSM.read outBuf i
      yield nextSamp)
{-# INLINE convolve #-}

foreign import ccall "convolve" conv_c :: CInt -> CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()
