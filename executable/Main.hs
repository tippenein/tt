module Main (main) where

import System.IO
import System.Environment
import Control.Concurrent (threadDelay)
import Control.Monad (when)

import Timer

fromT :: T -> Int
fromT (T h m s) = (h * 60 * 60) + (m * 60) + s

showCountdown :: T -> IO ()
showCountdown t = showCountdown' $ fromT t
  where
  showCountdown' 0 = print "done!"
  showCountdown' n = condP n >>
    threadDelay 1000000 >>
    showCountdown'(n - 1)

condP n = do
  hSetBuffering stdout NoBuffering
  when (n `mod` 30 == 0) $ do
    print' n

print' p = putStr $ show p ++ ".."

parseArgs :: [String] -> T
parseArgs [] = error "required time argument"
parseArgs (a:_) = toTime a

main :: IO ()
main = do
  a <- getArgs
  showCountdown $ parseArgs a
