module Main (main) where

-- import qualified TT
import System.Environment
import Text.Parsec

type Parser = Parsec String ()

data T = T { hours :: Int, minutes :: Int, seconds :: Int }
  deriving (Eq)

instance Show T where
  show (T h m s) = show h ++ " hours " ++ show m ++ " minutes " ++ show s ++ " seconds"

parse' :: Parser a -> String -> Either ParseError a
parse' rule = parse rule "(source_file)"

parseArgs :: [String] -> T
parseArgs [] = error "required time argument"
parseArgs (a:_) = toTime a

timeP :: Char -> Parser Int
timeP c = option 0 $ try (valParser <* char c)

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

spec :: IO ()
spec = do
  print $ show $ parseArgs ["1h22m3s"] == T 1 22 3
  print $ show $ parseArgs ["2m3s"] == T 0 2 3
  print $ show $ parseArgs ["2m"] == T 0 2 0
  return ()

main :: IO ()
main = do -- TT.timeit
  a <- getArgs
  print $ parseArgs a
