import Text.ParserCombinators.Parsec

p_query :: CharParser () [(String, Maybe String)]
p_query = p_pair `sepBy` char '&'

p_pair :: CharParser () (String, Maybe String)
p_pair = do
  name <- many1 p_char
  value <- optionMaybe (char '=' >> many p_char)
  return (name, value)

p_char :: CharParser () Char
p_char = oneOf urlBaseChars <|> (char '+' >> return ' ') <|> p_hex

urlBaseChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "$-_.!*'(),"

p_hex :: CharParser () Char
p_hex = undefined
  -- char '%'
  -- a <- hexDigit
  -- b <- hexDigit
  -- let ((d, _):_) = readHex [a, b]
  -- return . toEnum $ d
