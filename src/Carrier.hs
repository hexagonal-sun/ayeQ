{-# LANGUAGE ScopedTypeVariables #-}

module Carrier (CarrierOpts(..), run, parseOpts) where

import           Data.ByteString.Builder
import           Data.Complex
import           IQ
import           Options.Applicative
import           Pipes
import qualified Pipes.Prelude as P
import           Render
import           System.IO
import Control.Monad (replicateM_)


data CarrierOpts = CarrierOpts
  { carrierLength :: Int
  , outputFile :: String
  , sampleRate :: Int }

parseOpts :: Parser CarrierOpts
parseOpts = CarrierOpts
         <$> option auto
             ( long "length"
            <> short 'l'
            <> metavar "LENGTH"
            <> help "Carrier length in seconds" )
         <*> strOption
             ( long "out-file"
            <> short 'o'
            <> metavar "OUTPUT_FILE"
            <> help "Output IQ file")
         <*> option auto
             ( long "sample-rate"
            <> short 's'
            <> value 1000000
            <> showDefault
            <> metavar "SAMPLE_RATE"
            <> help "IQ Sample rate in Hz")


synthesize :: Monad m => Int -> Int -> Producer IQ m ()
synthesize sr length = replicateM_ n $ yield sample
  where n = sr * length
        sample = 1.0 :+ 0.0

run :: CarrierOpts -> IO ()
run opts = do
  putStr "Synthesizing Samples ..."
  hFlush stdout

  withBinaryFile (outputFile opts) WriteMode  $ \f ->
    runEffect $ synthesize (sampleRate opts) (carrierLength opts)
            >-> cfileSink f

  putStrLn "Done."
