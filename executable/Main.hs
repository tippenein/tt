module Main (main) where

-- import qualified TT
import System.Environment
import Text.Parsec
import Data.Maybe

type Parser = Parsec String ()

data T = T { hours :: Int, minutes :: Int, seconds :: Int }
  deriving Show

-- instance Show T where
--   show (T h m s) = show (fromMaybe 0 h) ++ " hours " ++ show (fromMaybe 0 m) ++ " minutes " ++ show (fromMaybe 0 s) ++ " seconds"

parse' :: Parser a -> String -> Either ParseError a
parse' rule = parse rule "(source_file)"

parseArgs :: [String] -> T
parseArgs [] = error "required time argument"
parseArgs (a:_) = toTime a


timeP :: Char -> Parser Int
timeP c = valParser <* char c

timeParser :: Parser T
timeParser = do
  h <- timeP 'h'
  m <- timeP 'm'
  s <- timeP 's'
  pure $ T h m s

toTime :: String -> T
toTime t = case parse' timeParser t of
  Left e -> error (show e)
  Right time' -> time'

valParser :: Parser Int
valParser = rd <$> many1 digit
  where rd = read :: String -> Int

main :: IO ()
main = do -- TT.timeit
  a <- getArgs
  print $ parseArgs a
