module Main (main) where

import System.Environment
import Control.Concurrent (threadDelay)

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

  condP n = if n `mod` 30 == 0 then (putStr $ show n ++ "..") else return ()


parseArgs :: [String] -> T
parseArgs [] = error "required time argument"
parseArgs (a:_) = toTime a

main :: IO ()
main = do
  a <- getArgs
  showCountdown $ parseArgs a
