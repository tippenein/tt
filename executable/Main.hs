module Main (main) where

import System.IO
import System.Environment
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Options.Applicative

import qualified Timer

showCountdown :: Options -> IO ()
showCountdown o = showCountdown' (Timer.fromT $ t o) (incr o)
  where
    showCountdown' 0 _ = print "done!"
    showCountdown' n i = condP n i >>
      threadDelay 1000000 >>
      showCountdown' (n - 1) (incr o)

condP n i = do
  hSetBuffering stdout NoBuffering
  when (n `mod` i == 0) $ do
    print' n

print' p = putStr $ show p ++ ".."

data Options
  = Options
  { incr :: Int
  , t :: Timer.T
  }
  
options :: Parser Options
options = Options
     <$> option auto
         ( long "increment"
        <> short 'i'
        <> value 30
        <> metavar "INC"
        <> help "how often to print the time" )
     <*> argument (str >>= mkTime)
         ( metavar "TIME"
        <> help "in the format #h#m#s or similar" )
  where
    mkTime s = pure $ Timer.toTime s

main :: IO ()
main = execParser opts >>= showCountdown
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "start a timer"
     <> header "tt - a tea timer" )
