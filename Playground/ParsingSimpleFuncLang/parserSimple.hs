import Control.Applicative
import Text.Parsec hiding (Empty)
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

import Text.Parsec.Expr

lexerConfig = emptyDef {Token.reservedOpNames = words "+ -"}

lexer = Token.makeTokenParser lexerConfig

reservedOp = Token.reservedOp lexer

integer = Token.integer lexer

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Num Integer
  deriving (Show)

value :: Parser Expr
value = do
  n <- integer
  return $ Num n

binary name label assoc =
  Infix
    (do reservedOp name
        return (\x y -> label x y))
    assoc

opTable = [[binary "+" Add AssocLeft, binary "-" Sub AssocLeft]]

expr :: Parser Expr
expr = buildExpressionParser opTable value

parseString :: Parser Expr -> String -> Either ParseError Expr
parseString e s = parse (e <* eof) "" s

parseFile :: Parser Expr -> FilePath -> IO (Either ParseError Expr)
parseFile e f = parseFromFile (e <* eof) f
