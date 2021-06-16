module Filter (FilterOptions(..), parseOpts, lowPass, convolve) where

import           Data.List
import           IQ
import           Data.Complex
import           Data.Foldable
import           Control.Monad.Primitive
import           Control.Monad.State.Strict
import qualified Data.Sequence as S
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
                | otherwise =  let n = fromIntegral x in (sin (fac * n * pi)) / (fac * n * pi)
        fac = 2 * (fromIntegral fc / fromIntegral sr)

lowPass :: Floating a => Int -> FilterOptions -> S.Seq a
lowPass sr opts = S.fromList x
  where nTaps = calculateNumTaps sr (transitionWidth opts)
        window = hann nTaps
        x = zipWith (*) window $ sinc nTaps sr (cutoff opts)

convolve :: (Monad m, Floating a) => S.Seq a -> Pipe a a m ()
convolve kernel = void $ flip runStateT (S.replicate (length kernel) 0.0) $ forever $ do
  x <- lift await
  s <- get
  let s' = S.insertAt 0 x $ S.deleteAt (S.length s - 1) s
      nextSamp = sum $  S.zipWith (*) s' kernel
  lift (yield nextSamp)
  put s'
{-# INLINE convolve #-}
