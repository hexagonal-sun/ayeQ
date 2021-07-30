{-# LANGUAGE FlexibleContexts #-}
module CW (parseOpts, run) where

import           Control.Monad
import           Control.Monad.Error.Class
import           Data.Char
import           Data.Complex
import           Data.List
import qualified Filter as F
import           IQ
import           Options.Applicative
import           Pipes
import qualified Pipes.Prelude as P
import           Render
import           System.IO
import           ZeroStuff

data CWOptions = CWOptions
  { sampleRate :: Int
  , wpm :: Int
  , text :: String
  , outputFile :: String }

parseOpts :: Parser CWOptions
parseOpts = CWOptions
         <$> option auto
             ( long "sample-rate"
            <> short 's'
            <> metavar "SAMPLE_RATE"
            <> help "Sample rate of the produced IQ file" )
         <*> option auto
             ( long "wpm"
            <> short 'w'
            <> value 25
            <> showDefault
            <> metavar "WPM_RATE"
            <> help "Rate of keying in Words per Minute" )
         <*> strOption
             ( long "in-text"
            <> short 't'
            <> metavar "INPUT_TEXT"
            <> help "Input text message" )
         <*> strOption
             ( long "out-file"
            <> short 'o'
            <> metavar "OUTPUT_FILE"
            <> help "Output IQ file" )

data CW = Dah
        | Dit

cwMap :: MonadError String m => Char -> m [CW]
cwMap c
  | c == 'a' = pure [ Dah, Dit ]
  | c == 'b' = pure [ Dah, Dit, Dit, Dit ]
  | c == 'c' = pure [ Dah, Dit, Dah, Dit ]
  | c == 'd' = pure [ Dah, Dit, Dit ]
  | c == 'e' = pure [ Dit ]
  | c == 'f' = pure [ Dit, Dit, Dah, Dit ]
  | c == 'g' = pure [ Dah, Dah, Dit ]
  | c == 'h' = pure [ Dit, Dit, Dit, Dit ]
  | c == 'i' = pure [ Dit, Dit ]
  | c == 'j' = pure [ Dit, Dah, Dah, Dah ]
  | c == 'k' = pure [ Dah, Dit, Dah ]
  | c == 'l' = pure [ Dit, Dah, Dit, Dit ]
  | c == 'm' = pure [ Dah, Dah ]
  | c == 'n' = pure [ Dah, Dit ]
  | c == 'o' = pure [ Dah, Dah, Dah ]
  | c == 'p' = pure [ Dit, Dah, Dah, Dit ]
  | c == 'q' = pure [ Dah, Dah, Dit, Dah ]
  | c == 'r' = pure [ Dit, Dah, Dit ]
  | c == 's' = pure [ Dit, Dit, Dit ]
  | c == 't' = pure [ Dah ]
  | c == 'u' = pure [ Dit, Dit, Dah ]
  | c == 'v' = pure [ Dit, Dit, Dit, Dah ]
  | c == 'w' = pure [ Dit, Dah, Dah ]
  | c == 'x' = pure [ Dah, Dit, Dit, Dah ]
  | c == 'y' = pure [ Dah, Dit, Dah, Dah ]
  | c == 'z' = pure [ Dah, Dah, Dit, Dit ]
  | c == '1' = pure [ Dit, Dah, Dah, Dah, Dah ]
  | c == '2' = pure [ Dit, Dit, Dah, Dah, Dah ]
  | c == '3' = pure [ Dit, Dit, Dit, Dah, Dah ]
  | c == '4' = pure [ Dit, Dit, Dit, Dit, Dah ]
  | c == '5' = pure [ Dit, Dit, Dit, Dit, Dit ]
  | c == '6' = pure [ Dah, Dit, Dit, Dit, Dit ]
  | c == '7' = pure [ Dah, Dah, Dit, Dit, Dit ]
  | c == '8' = pure [ Dah, Dah, Dah, Dit, Dit ]
  | c == '9' = pure [ Dah, Dah, Dah, Dah, Dit ]
  | c == '0' = pure [ Dah, Dah, Dah, Dah, Dah ]
  | otherwise = throwError $ "Unknown CW character: '" ++ [c] ++ "'"

data KeyState = On | Off
data Keying = Keying
  { units :: Int
  , keyState :: KeyState }

cwKeyingMap :: CW -> Keying
cwKeyingMap Dah = Keying 3 On
cwKeyingMap Dit = Keying 1 On

keyLetter :: MonadError String m => Char -> m [Keying]
keyLetter l = intersperse (Keying 1 Off) <$> (map cwKeyingMap <$> cwMap l)

keyWord :: MonadError String m => String -> m [Keying]
keyWord word = intercalate  [Keying 3 Off] <$> sequence (keyLetter <$> word)

keyText :: String -> Either String [Keying]
keyText text = intercalate [Keying 7 Off] <$> sequence (keyWord <$> words text)

synthesizeKeying :: MonadIO m => Int -> Int -> Pipe Keying IQ m ()
synthesizeKeying sr wpm = forever $ do
  keying <- await

  let iq = case keying of
        Keying _ On -> 1.0 :+ 0.0
        Keying _ Off  -> 0.0 :+ 0.0
      n = round $ fromIntegral sr * (60.0 / (50.0 * fromIntegral wpm)) * fromIntegral (units keying)
  replicateM_ n $ yield iq

run :: CWOptions -> IO ()
run opts = case keyText $ toLower <$> text opts of
  Left err -> putStr err
  Right keyings -> do
    putStr "Synthesizing samples... "
    hFlush stdout

    let kernel = F.lowPass (sampleRate opts) $ F.FilterOptions 100 1000

    withBinaryFile (outputFile opts) WriteMode $ \f ->
      runEffect $ each keyings
           >-> synthesizeKeying (sampleRate opts) (wpm opts)
           >-> F.convolve kernel
           >-> cfileSink f

    putStrLn "Done."

