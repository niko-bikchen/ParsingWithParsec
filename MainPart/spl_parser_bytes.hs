{-
Name: spl_parser_bytes.hs
Author: Nikolay Bikchentaev
Version: 1.0
===========================
This program parses SPL source code
represented as ByteString using predifined
Parsec functions
-}
{-# OPTIONS_GHC -Wall #-}

module SPLParserBytes where

import Control.Monad
import qualified Data.ByteString.Char8 as BC
import Text.Parsec
import Text.Parsec.ByteString

-- Type for variable value
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

-- Type for variable type
data Type
  = It
  | Bt
  deriving (Show, Eq)

-- Alias for statement
type Program = Stmt

-- General grammar (keywords, operators, etc.)
{-
  symbol = ';' | '{' | '}' | '(' | ')' 
  idenName=  char {digit | char}
  keyword= "int" | "bool" | "if" | "while" | "for" | "else" | "true" | "false"
  iden   =  idenName .... not "int" "bool" "if" "while" "for" "else" "true" "false"
  number = digit { digit }.
  mulOrDivOp  = "*" | "/".
  addOrSubOp  = "+" | "-".
  relOp  = "<" | "<=" | ">" | ">=" | "==" 
  andOp  = "&" 
  orOp  = "|"
  idenType  = "int" | "bool" 
-}
whitespaces :: Parser ()
whitespaces = void $ many $ oneOf " \n\t"

lexem :: Parser a -> Parser a
lexem parser = do
  res <- parser
  whitespaces
  return res

symbol :: Char -> Parser ()
symbol chr = void $ lexem $ char chr

keyword :: String -> Parser ()
keyword str = try $ lexem $ string str >> notFollowedBy alphaNum

oprtr :: String -> Bop -> Parser Bop
oprtr str bop = do
  void $ string str
  whitespaces
  return bop

exprOp :: Parser Bop -> Parser (Exp -> Exp -> Exp)
exprOp p = do
  x <- lexem p
  return (flip Op x)

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- many1 digit
  return $ read (s ++ cs)

idenName :: Parser String
idenName = do
  whitespaces
  a <- letter
  cs <- many (letter <|> digit)
  return (a : cs)

iden :: Parser String
iden =
  try $ do
    name <- idenName
    if name `elem`
       ["int", "bool", "if", "while", "for", "else", "True", "False"]
      then unexpected ("use of reserved word " ++ show name)
      else return name

idenType :: Parser Type
idenType =
  (do keyword "int"
      return It) <|>
  (do keyword "bool"
      return Bt)

mulOrDivOp :: Parser Bop
mulOrDivOp = oprtr "*" Times <|> oprtr "/" Div

addOrSubOp :: Parser Bop
addOrSubOp = try (oprtr "+" Plus) <|> try (oprtr "-" Minus)

andOp :: Parser Bop
andOp = oprtr "&" Ba

orOp :: Parser Bop
orOp = oprtr "|" Bo

relOp :: Parser Bop
relOp =
  try (oprtr ">=" Ge) <|> try (oprtr ">" Gt) <|> try (oprtr "<=" Le) <|>
  try (oprtr "==" Eql) <|>
  try (oprtr "<" Lt)

-- Expression grammar
{-
  factor = '(' expr ')' | number | "true" | "false" | iden
  term   = factor { mulOrDivOp factor }
  relat  = term { addOrSubOp term }
  conj   = relat [relOp relat]
  disj   = conj { orOp conj}
  expr   = disj { andOp disj}
-}
bopPrsr :: Parser Exp -> Parser Bop -> Parser Exp
bopPrsr prsr bin = do
  x <- prsr
  rest x
  where
    rest x =
      (do b <- bin
          y <- prsr
          rest $ Op x b y) <|>
      return x

factor :: Parser Exp
factor =
  (do symbol '('
      x <- expr
      symbol ')'
      return x) <|>
  (do nm <- lexem number
      return (Const (I nm))) <|>
  (do keyword "true"
      return (Const (B True))) <|>
  (do keyword "false"
      return (Const (B False))) <|>
  (do cs <- lexem iden
      return (Var cs) <?> "factor")

term :: Parser Exp
term = bopPrsr factor mulOrDivOp

relat :: Parser Exp
relat = bopPrsr term addOrSubOp

conj :: Parser Exp
conj = bopPrsr relat relOp

disj :: Parser Exp
disj = bopPrsr conj orOp

expr :: Parser Exp
expr = bopPrsr disj andOp

-- Statement grammar
{-
  stmt   = "for" forSt | "while" whileSt | "if" ifSt 
         | iden assSt | blockSt  
  forSt  = '(' stmt ';' expr ';' stmt ')' stmt 
  whileSt= '(' expr ')' stmt  
  ifSt   = '(' expr ')' stmt "else" stmt 
  assSt  = "++" | ":=" expr 
  blockSt= '{' {defin} listSt '}' 
  defin  = type iden ';'
  listSt = stmt {';' stmt}  
  program= stmt eos 
-}
stmt :: Parser Stmt
stmt =
  (do keyword "for"
      forSt) <|>
  (do keyword "while"
      whileSt) <|>
  (do keyword "if"
      ifSt) <|>
  (do var <- lexem iden
      assignSt var) <|>
  (blockSt <?> "statement")

forSt :: Parser Stmt
forSt = do
  symbol '('
  st1 <- stmt
  symbol ';'
  ex <- expr
  symbol ';'
  st2 <- stmt
  symbol ')'
  For st1 ex st2 <$> stmt

whileSt :: Parser Stmt
whileSt = do
  symbol '('
  ex <- expr
  symbol ')'
  While ex <$> stmt

ifSt :: Parser Stmt
ifSt = do
  symbol '('
  ex <- expr
  symbol ')'
  st1 <- stmt
  void $ string "else"
  If ex st1 <$> stmt

assignSt :: String -> Parser Stmt
assignSt varName =
  (do void $ string "++"
      return $ Incr varName) <|>
  (do void $ string ":="
      whitespaces
      Assign varName <$> expr)

defin :: Parser (String, Type)
defin = do
  tp <- idenType
  idn <- idenName
  symbol ';'
  return (idn, tp)

listStmt :: Parser [Stmt]
listStmt = stmt `sepBy` (symbol ';')

blockSt :: Parser Stmt
blockSt = do
  symbol '{'
  defins <- many defin
  stmts <- listStmt
  symbol '}'
  return $ Block defins stmts

program :: Parser Stmt
program = do
  spaces
  r <- stmt
  eof
  return r

parseSPLByteString :: BC.ByteString -> Either ParseError Stmt
parseSPLByteString = parse program ""

-- Parses program source code
startParser :: String -> Either ParseError Stmt
startParser text =
  case parseSPLByteString (BC.pack text) of
    Right res -> Right res
    Left err -> Left err

-- Parses text from a file
parseFile :: String -> IO Stmt
parseFile file = do
  program_txt <- BC.readFile file
  case parseSPLByteString program_txt of
    Left e -> print e >> fail "parse error"
    Right r -> return r

-- Test data
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
