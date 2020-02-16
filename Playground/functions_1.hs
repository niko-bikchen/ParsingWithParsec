{-# LANGUAGE FlexibleContexts #-}

-- I import qualified so that it's clear which
-- functions are from the parsec library:
import qualified Text.Parsec as Parsec

-- I am the choice and optional error message infix operator, used later:
import Text.Parsec ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import Control.Applicative

-- Get the Identity monad from here:
import Control.Monad.Identity (Identity)

-- alias parseTest for more concise usage in my examples:
parse rule text = Parsec.parse rule "(source)" text

main = do
  let result = parse (Parsec.char 'H') "Hello"
  case result of
    Right v -> putStrLn "success!"
    Left err -> putStrLn ("whoops, error: " ++ show err)

-- This looks for letters, then spaces, then digits.
-- we then return letters and digits in a tuple.
myParser :: Parsec.Parsec String () (String, String)
myParser = do
  letters <- Parsec.many1 Parsec.letter
  Parsec.spaces
  digits <- Parsec.many1 Parsec.digit
  return (letters, digits)

mySeparator :: Parsec.Parsec String () ()
mySeparator = do
  Parsec.spaces
  Parsec.char ','
  Parsec.spaces

myPairs :: Parsec.Parsec String () [(String, String)]
myPairs =
  Parsec.many $ do
    pair <- myParser
    mySeparator
    return pair

myPairs2a :: Parsec.Parsec String () [(String, String)]
myPairs2a = Parsec.endBy myParser mySeparator

myPairs2b :: Parsec.Parsec String () [(String, String)]
myPairs2b = Parsec.sepBy myParser mySeparator

myPairs2 :: Parsec.Parsec String () [(String, String)]
myPairs2 =
  Parsec.many $ do
    pair <- myParser
    Parsec.eof <|> mySeparator
    return pair

helloOrHowdy :: Parsec.Parsec String () String
helloOrHowdy = do
  first <- Parsec.char 'h'
  rest <- Parsec.string "ello" <|> Parsec.string "owdy"
  return (first : rest)

helloOrHowdy2 :: Parsec.Parsec String () String
helloOrHowdy2 = Parsec.try (Parsec.string "hello") <|> Parsec.string "howdy"

-- in applicative style:
myParserApp :: Parsec.Parsec String () (String, String)
myParserApp =
  (,) <$> Parsec.many1 Parsec.letter <*>
  (Parsec.spaces *> Parsec.many1 Parsec.digit)

-- could also be written as:
myParserApp2 :: Parsec.Parsec String () (String, String)
myParserApp2 =
  liftA2
    (,)
    (Parsec.many1 Parsec.letter)
    (Parsec.spaces *> Parsec.many1 Parsec.digit)

-- or even (swapping *> for the more familiar >>):
myParserApp3 :: Parsec.Parsec String () (String, String)
myParserApp3 =
  liftA2
    (,)
    (Parsec.many1 Parsec.letter)
    (Parsec.spaces >> Parsec.many1 Parsec.digit)
