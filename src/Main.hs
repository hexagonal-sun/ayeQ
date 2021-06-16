module Main where

import qualified AM
import qualified Carrier
import           Options.Applicative
import           Control.Monad

opts :: Parser (IO ())
opts = subparser
     ( command "am" (info (AM.run <$> AM.parseOpts <**> helper) (progDesc "Modulate a sound file with amplitude modulation"))
    <> command "carrier" (info (Carrier.run <$> Carrier.parseOpts <**> helper) (progDesc "Synthesize a carrier at baseband")))

main :: IO ()
main = join $ execParser (info (helper <*> opts)
                         ( fullDesc
                         <> progDesc "An IQ toolbox for producing various kinds of IQ files."
                         <> header "IQ toolbox" ))
