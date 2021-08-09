{-# LANGUAGE ScopedTypeVariables #-}

module AM (AMOptions(..), run, parseOpts) where

import           Data.ByteString.Builder
import           Data.Complex
import qualified Data.Vector.Storable as V
import           IQ
import qualified Filter as F
import           Pipes
import qualified Pipes.Prelude as P
import           Control.Monad
import           ZeroStuff
import           Render
import           Options.Applicative
import qualified Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer.Vector as SV
import           System.IO

data AMOptions = AMOptions
  { inputFile :: String
  , outputFile :: String
  , modIndex :: Int
  , upsample :: Int
  , filterOpts :: F.FilterOptions }

parseOpts :: Parser AMOptions
parseOpts = AMOptions
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
             ( long "modulation-index"
            <> short 'm'
            <> value 100
            <> showDefault
            <> metavar "MOD_INDEX"
            <> help "Modulation index, value between 1-100")
         <*> option auto
             ( long "upsample-factor"
            <> short 'u'
            <> value 10
            <> showDefault
            <> metavar "UPSAMPLE_FAC"
            <> help "Upsample factor, outSR = inSR * UPSAMPLE_FAC")
         <*> F.parseOpts

modulateAM' :: Int -> Float -> IQ
modulateAM' modIdx s = (base + amp) :+ 0.0
  where x = fromIntegral modIdx / 200
        amp = s * x
        base = 1 - x

modulateAM :: Monad m => Int -> Pipe Float IQ m ()
modulateAM x = P.map $ modulateAM' x

yieldEvery :: Monad m => Int -> Pipe a a m ()
yieldEvery n = do
  replicateM_ (n - 1) await
  await >>= yield
  yieldEvery n

run :: AMOptions -> IO ()
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
            >-> modulateAM (modIndex opts)
            >-> zeroStuff (upsample opts)
            >-> F.convolve filterKernel
            >-> cfileSink f

  putStrLn "Done."

  putStrLn $  "CFile sample rate: " ++ show  upsampleRate ++ "Hz"
