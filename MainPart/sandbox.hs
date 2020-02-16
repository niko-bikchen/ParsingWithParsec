import Control.Monad (void)
import Text.ParserCombinators.Parsec

data Bexp
  = Bvalue Bool
  | Bvar Char
  | Not Bexp
  | And Bexp Bexp
  | Or Bexp Bexp
  deriving (Eq, Show)

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

symbol :: Char -> Parser ()
symbol c = void $ lexeme $ char c

-- Recursively applies *parseBraces* function
parseBracesHelper :: Parser ()
parseBracesHelper = do
  whitespace
  parseBraces '(' ')' <|> parseBraces '[' ']' <|> parseBraces '{' '}'

parseBraces :: Char -> Char -> Parser ()
parseBraces ob cb = do
  symbol ob
  -- checking wheteher we have nested braces
  parseBracesHelper <|> whitespace -- *whitespace* is used to continue program execution if the check fails
  symbol cb
  -- checking wheteher we have more braces on this "level"
  parseBracesHelper <|> whitespace -- *whitespace* is used to continue program execution if the check fails

fullBrace :: Parser ()
fullBrace = do
  parseBracesHelper
  eof

balance :: String -> Bool
balance str = either (const False) (const True) (parse fullBrace "" str)

fullBe :: Parser Bexp
fullBe = undefined

anBexp :: String -> Maybe Bexp
anBexp str =
  case (parse fullBe "" str) of
    Left _ -> Nothing
    Right ex -> Just ex