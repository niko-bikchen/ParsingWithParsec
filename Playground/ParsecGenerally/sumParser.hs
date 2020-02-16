import System.IO
import Text.ParserCombinators.Parsec

plus :: Parser ()
plus = do
  spaces
  char '+'
  spaces

parseExpr :: Parser [String]
parseExpr = sepBy (many1 digit) plus

main :: IO ()
main = do
  expr <- getLine
  case parse parseExpr "(stdin)" expr of
    Left e -> do
      putStrLn "Error parsing input:"
      print e
    Right r -> print $ sum $ map read r
