module Timer where

import Text.Parsec

type Parser = Parsec String ()

data T = T { hours :: Int, minutes :: Int, seconds :: Int }
  deriving (Show, Eq)

parse' :: Parser a -> String -> Either ParseError a
parse' rule = parse rule "(source_file)"

timeP :: Char -> Parser Int
timeP c = option 0 $ try (valParser <* char c)

fromT :: T -> Int
fromT (T h m s) = (h * 60 * 60) + (m * 60) + s

timeParser :: Parser T
timeParser = do
  h <- timeP 'h'
  m <- timeP 'm'
  s <- timeP 's'
  pure $ T h m s

valParser :: Parser Int
valParser = rd <$> many1 digit
  where rd = read :: String -> Int

toTime :: String -> T
toTime t = case parse' timeParser t of
  Left e -> error (show e)
  Right time' -> time'
