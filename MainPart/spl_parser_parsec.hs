{-
Name: spl_parser_parsec.hs
Author: Nikolay Bikchentaev
Version: 1.0
===========================
This program parses SPL source code
using Parsec Expr and Language modules
-}
{-# OPTIONS_GHC -Wall #-}

module SPLParserParsec where

import Control.Monad
import Data.Functor.Identity
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Value
  = I Int
  | B Bool
  deriving (Show, Eq)

data Exp
  = Var String
  | Const Value
  | Op Exp Bop Exp
  deriving (Show, Eq)

data Bop
  = Plus
  | Minus
  | Times
  | Div
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  | Ba
  | Bo
  deriving (Show, Eq)

data Stmt
  = Assign String Exp
  | Incr String
  | If Exp Stmt Stmt
  | While Exp Stmt
  | For Stmt Exp Stmt Stmt
  | Block [(String, Type)] [Stmt]
  deriving (Show, Eq)

data Type
  = It
  | Bt
  deriving (Show, Eq)

type Program = Stmt

-- Keywords
rNames :: [String]
rNames = words "true false bool int if else while for"

-- Operators
opNames :: [String]
opNames = words "+ - * / < <= > >= := == & | ++"

-- Language definition
languageDef :: GenLanguageDef String u Data.Functor.Identity.Identity
languageDef =
  emptyDef
    { Token.commentStart = "/*"
    , Token.commentEnd = "*/"
    , Token.commentLine = "//"
    , Token.identStart = letter
    , Token.identLetter = alphaNum <|> char '_'
    , Token.reservedNames = rNames
    , Token.reservedOpNames = opNames
    }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer

reserved = Token.reserved lexer

reservedOp = Token.reservedOp lexer

parens = Token.parens lexer

integer = Token.integer lexer

semi = Token.semi lexer

whiteSpace = Token.whiteSpace lexer

symbol = Token.symbol lexer

stmt :: Parser Stmt
stmt =
  forStmt <|> whileStmt <|> ifStmt <|>
  (do var <- identifier
      assignSt var) <|>
  blockStmt <?> "statement"

forStmt :: Parser Stmt
forStmt = do
  reserved "for"
  void $ symbol "("
  st1 <- stmt
  void $ symbol ";"
  cond <- expr
  void $ symbol ";"
  st2 <- stmt
  void $ symbol ")"
  For st1 cond st2 <$> stmt

whileStmt :: Parser Stmt
whileStmt = do
  reserved "while"
  cond <- parens expr
  While cond <$> stmt

ifStmt :: Parser Stmt
ifStmt = do
  reserved "if"
  cond <- parens expr
  reserved "then"
  st <- stmt
  reserved "else"
  If cond st <$> stmt

assignSt :: String -> Parser Stmt
assignSt var =
  (do reservedOp "++"
      return $ Incr var) <|>
  (do reservedOp ":="
      Assign var <$> expr)

defin :: Parser (String, Type)
defin = do
  tp <- (reserved "int" >> return It) <|> (reserved "bool" >> return Bt)
  iden <- identifier
  void $ symbol ";"
  return (iden, tp)

listStmt :: Parser [Stmt]
listStmt = stmt `sepBy` (symbol ";")

blockStmt :: Parser Stmt
blockStmt = do
  void $ symbol "{"
  defs <- many defin
  sts <- listStmt
  void $ symbol "}"
  return $ Block defs sts

operators =
  [ [ Infix (reservedOp "*" >> return (\x y -> Op x Times y)) AssocLeft
    , Infix (reservedOp "/" >> return (\x y -> Op x Div y)) AssocLeft
    ]
  , [ Infix (reservedOp "+" >> return (\x y -> Op x Plus y)) AssocLeft
    , Infix (reservedOp "-" >> return (\x y -> Op x Minus y)) AssocLeft
    ]
  , [ Infix (reservedOp "==" >> return (\x y -> Op x Eql y)) AssocNone
    , Infix (reservedOp "<" >> return (\x y -> Op x Lt y)) AssocNone
    , Infix (reservedOp "<=" >> return (\x y -> Op x Le y)) AssocNone
    , Infix (reservedOp ">" >> return (\x y -> Op x Gt y)) AssocNone
    , Infix (reservedOp ">=" >> return (\x y -> Op x Ge y)) AssocNone
    ]
  , [ Infix (reservedOp "&" >> return (\x y -> Op x Ba y)) AssocLeft
    , Infix (reservedOp "|" >> return (\x y -> Op x Bo y)) AssocLeft
    ]
  ]

term :: Parser Exp
term =
  parens expr <|> (Const . I . fromIntegral) <$> integer <|>
  (reserved "true" >> return (Const (B True))) <|>
  (reserved "false" >> return (Const (B False))) <|>
  liftM Var identifier

expr :: Parser Exp
expr = buildExpressionParser operators term

startParser :: String -> Either ParseError Stmt
startParser = parse (whiteSpace >> stmt) ""

power :: String
power =
  "{ int b; int e; int out; b := 6; e := 5; out:= 1;\
   \  {int i; for (i:=0; i<e; i++) out := out*b}   \
   \}"

powerAST :: Program
powerAST =
  Block
    [("b", It), ("e", It), ("out", It)]
    [ Assign "b" (Const (I 6))
    , Assign "e" (Const (I 5))
    , Assign "out" (Const (I 1))
    , Block
        [("i", It)]
        [ For
            (Assign "i" (Const (I 0)))
            (Op (Var "i") Lt (Var "e"))
            (Incr "i")
            (Assign "out" (Op (Var "out") Times (Var "b")))
        ]
    ]

squareRoot :: String
squareRoot =
  "{int a; int b; a := 317; b := 0;\
   \  {bool c; c:=true; while(c) {b++; c:= a >= b*b}};\
   \  b := b-1\
   \ }"

squareRootAST :: Program
squareRootAST =
  Block
    [("a", It), ("b", It)]
    [ Assign "a" (Const (I 317))
    , Assign "b" (Const (I 0))
    , Block
        [("c", Bt)]
        [ Assign "c" (Const (B True))
        , While
            (Var "c")
            (Block
               []
               [ (Incr "b")
               , Assign "c" (Op (Var "a") Ge (Op (Var "b") Times (Var "b")))
               ])
        ]
    , Assign "b" (Op (Var "b") Minus (Const (I 1)))
    ]
