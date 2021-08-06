{-# LANGUAGE ScopedTypeVariables #-}

module FM (FMOptions(..), run, parseOpts) where

import           Control.Monad
import           Control.Monad.State.Lazy (StateT(runStateT), MonadState (get, put))
import           Data.Complex
import qualified Data.Vector.Storable as V
import qualified Filter as F
import           IQ
import           Options.Applicative
import           Pipes
import           Render
import qualified Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer.Vector as SV
import           System.IO
import           ZeroStuff

data FMOptions = FMOptions
  { inputFile :: String
  , outputFile :: String
  , deviationRatio :: Double
  , upsample :: Int
  , filterOpts :: F.FilterOptions }

parseOpts :: Parser FMOptions
parseOpts = FMOptions
         <$> strOption
             ( long "in-file"
            <> short 'i'
            <> metavar "INPUT_FILE"
            <> help "Input sound file" )
         <*> strOption
             ( long "out-file"
            <> short 'o'
            <> metavar "OUTPUT_FILE"
            <> help "Output IQ file")
         <*> option auto
             ( long "deviation-ratio"
            <> short 'd'
            <> value 1
            <> showDefault
            <> metavar "DEV_RATIO"
            <> help "The raio of max_devation over max_mod_frequency.")
         <*> option auto
             ( long "upsample-factor"
            <> short 'u'
            <> value 10
            <> showDefault
            <> metavar "UPSAMPLE_FAC"
            <> help "Upsample factor, outSR = inSR * UPSAMPLE_FAC")
         <*> F.parseOpts

-- clamp phase to [-pi, pi]
clampPhase :: Double -> Double
clampPhase phase
  | phase > pi = clampPhase $ phase - (2 * pi)
  | phase < -pi = clampPhase $ phase + (2 * pi)
  | otherwise =  phase

modulateFM :: MonadIO  m => Double -> Pipe Float IQ m ()
modulateFM devRatio = void $ flip runStateT (0 :: Double) $ forever $ do
  sample <- lift await
  phase <- get
  let phase' = clampPhase $ phase + realToFrac sample * devRatio
  lift . yield $ realToFrac (cos phase') :+ realToFrac (sin phase')
  put phase'

yieldEvery :: Monad m => Int -> Pipe a a m ()
yieldEvery n = do
  replicateM_ (n - 1) await
  await >>= yield
  yieldEvery n

run :: FMOptions -> IO ()
run opts = do
  (info, Just (aSamps :: SV.Buffer Float)) <- SF.readFile $ inputFile opts

  let sampsVec = SV.fromBuffer aSamps
      upsampleRate = upsample opts * SF.samplerate info
      filterKernel = F.lowPass upsampleRate $ filterOpts opts

  putStr "Processing samples... "
  hFlush stdout

  withBinaryFile (outputFile opts) WriteMode  $ \f ->
    runEffect $ V.mapM_ yield sampsVec
            >-> yieldEvery (SF.channels info)
            >-> modulateFM (deviationRatio opts)
            >-> zeroStuff (upsample opts)
            >-> F.convolve filterKernel
            >-> cfileSink f

  putStrLn "Done."

  putStrLn $  "CFile sample rate: " ++ show  upsampleRate ++ "Hz"
