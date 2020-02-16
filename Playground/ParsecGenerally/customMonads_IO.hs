import Control.Monad.IO.Class
import Text.Parsec

play :: String -> IO (Either ParseError Integer)
play s = runParserT pmain () "parameter" s

pmain :: ParsecT [Char] () IO Integer
pmain = do
  x <- pnum `chainl1` pplus
  eof
  return x

pnum = do
  x <- read `fmap` many1 digit
  liftIO (putStrLn "bling!")
  return x

pplus = char '+' >> return (+)
