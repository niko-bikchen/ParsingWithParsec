{-
Name: spl_custom_alex.hs
Author: Nikolay Bikchentaev
Version: 1.0
===========================
This program parses SPL source code
by decomposing it into custom tokens
which we define in the Alex source file
named "AlexLexer.x". In this file we 
spicify our lexer and tokens it will
generate. This file is used by Alex
to generate .hs file with the lexer.
-}
{-# OPTIONS_GHC -Wall #-}

module SPLAlex where

import AlexLexer
import Control.Monad
import qualified Text.Parsec as Parsec
import Text.ParserCombinators.Parsec

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

updatePos :: SourcePos -> DataToken -> [DataToken] -> SourcePos
updatePos pos _ (tok:_) =
  setSourceLine (setSourceColumn pos (column tok)) (line tok)
updatePos pos _ [] = pos

satisfyT ::
     (Monad m)
  => (DataToken -> Bool)
  -> Parsec.ParsecT [DataToken] u m DataToken
satisfyT f =
  tokenPrim
    show
    updatePos
    (\c ->
       if f c
         then Just c
         else Nothing)

bopPrsr ::
     Parsec.Parsec [DataToken] () Exp
  -> Parsec.Parsec [DataToken] () Bop
  -> Parsec.Parsec [DataToken] () Exp
bopPrsr prsr bin = do
  x <- prsr
  rest x
  where
    rest x =
      (do b <- bin
          y <- prsr
          rest $ Op x b y) <|>
      return x

matchKeyword :: String -> Parsec.Parsec [DataToken] () DataToken
matchKeyword keyword = satisfyT (== KeywordToken 0 0 keyword)

matchDelimiter :: String -> Parsec.Parsec [DataToken] () DataToken
matchDelimiter delimiter = satisfyT (== DelimiterSymbolToken 0 0 delimiter)

matchArithmOp :: String -> Parsec.Parsec [DataToken] () DataToken
matchArithmOp op = satisfyT (== ArithmOpToken 0 0 op)

matchAssignOp :: Parsec.Parsec [DataToken] () DataToken
matchAssignOp = satisfyT (== AssignOpToken 0 0 ":=")

matchBoolOp :: String -> Parsec.Parsec [DataToken] () DataToken
matchBoolOp op = satisfyT (== BoolOpToken 0 0 op)

matchBoolConst :: Bool -> Parsec.Parsec [DataToken] () DataToken
matchBoolConst val = satisfyT (== BoolConstToken 0 0 val)

matchNumber :: Parsec.Parsec [DataToken] () DataToken
matchNumber = satisfyT (== NumConstToken 0 0 0)

matchVarName :: Parsec.Parsec [DataToken] () DataToken
matchVarName = satisfyT (== VarNameToken 0 0 "")

matchVarType :: String -> Parsec.Parsec [DataToken] () DataToken
matchVarType tp = satisfyT (== VarTypeToken 0 0 tp)

arithmOpBop :: String -> Bop -> Parsec.Parsec [DataToken] () Bop
arithmOpBop op bop = do
  void $ matchArithmOp op
  return bop

boolOpBop :: String -> Bop -> Parsec.Parsec [DataToken] () Bop
boolOpBop op bop = do
  void $ matchBoolOp op
  return bop

mulOrDivOp :: Parsec.Parsec [DataToken] () Bop
mulOrDivOp = arithmOpBop "*" Times <|> arithmOpBop "/" Div

addOrSubOp :: Parsec.Parsec [DataToken] () Bop
addOrSubOp = try (arithmOpBop "+" Plus) <|> try (arithmOpBop "-" Minus)

andOp :: Parsec.Parsec [DataToken] () Bop
andOp = boolOpBop "&" Ba

orOp :: Parsec.Parsec [DataToken] () Bop
orOp = boolOpBop "|" Bo

relOp :: Parsec.Parsec [DataToken] () Bop
relOp =
  try (boolOpBop ">=" Ge) <|> try (boolOpBop ">" Gt) <|> try (boolOpBop "<=" Le) <|>
  try (boolOpBop "==" Eql) <|>
  try (boolOpBop "<" Lt)

factor :: Parsec.Parsec [DataToken] () Exp
factor =
  (do void $ matchDelimiter "("
      x <- expr
      void $ matchDelimiter ")"
      return x) <|>
  (do num <- numValue <$> matchNumber
      return (Const (I num))) <|>
  (do void $ matchBoolConst True
      return (Const (B True))) <|>
  (do void $ matchBoolConst False
      return (Const (B False))) <|>
  (do name <- varName <$> matchVarName
      return (Var name) <?> "factor")

term :: Parsec.Parsec [DataToken] () Exp
term = bopPrsr factor mulOrDivOp

relat :: Parsec.Parsec [DataToken] () Exp
relat = bopPrsr term addOrSubOp

conj :: Parsec.Parsec [DataToken] () Exp
conj = bopPrsr relat relOp

disj :: Parsec.Parsec [DataToken] () Exp
disj = bopPrsr conj orOp

expr :: Parsec.Parsec [DataToken] () Exp
expr = bopPrsr disj andOp

forSt :: Parsec.Parsec [DataToken] () Stmt
forSt = do
  void $ matchDelimiter "("
  st1 <- stmt
  void $ matchDelimiter ";"
  ex <- expr
  void $ matchDelimiter ";"
  st2 <- stmt
  void $ matchDelimiter ")"
  For st1 ex st2 <$> stmt

whileSt :: Parsec.Parsec [DataToken] () Stmt
whileSt = do
  void $ matchDelimiter "("
  ex <- expr
  void $ matchDelimiter ")"
  While ex <$> stmt

ifSt :: Parsec.Parsec [DataToken] () Stmt
ifSt = do
  void $ matchDelimiter "("
  ex <- expr
  void $ matchDelimiter ")"
  st1 <- stmt
  void $ matchKeyword "else"
  If ex st1 <$> stmt

assignSt :: String -> Parsec.Parsec [DataToken] () Stmt
assignSt name =
  (do void $ matchArithmOp "++"
      return $ Incr name) <|>
  (do void matchAssignOp
      Assign name <$> expr)

iden :: Parsec.Parsec [DataToken] () String
iden =
  try $ do
    name <- varName <$> matchVarName
    if name `elem`
       ["int", "bool", "if", "while", "for", "else", "True", "False"]
      then unexpected ("use of reserved word " ++ show name)
      else return name

idenType :: Parsec.Parsec [DataToken] () Type
idenType =
  (do void $ matchVarType "int"
      return It) <|>
  (do void $ matchVarType "bool"
      return Bt)

defin :: Parsec.Parsec [DataToken] () (String, Type)
defin = do
  tp <- idenType
  name <- varName <$> matchVarName
  void $ matchDelimiter ";"
  return (name, tp)

listStmt :: Parsec.Parsec [DataToken] () [Stmt]
listStmt = stmt `sepBy` (matchDelimiter ";")

blockSt :: Parsec.Parsec [DataToken] () Stmt
blockSt = do
  void $ matchDelimiter "{"
  defins <- many defin
  stmts <- listStmt
  void $ matchDelimiter "}"
  return $ Block defins stmts

stmt :: Parsec.Parsec [DataToken] () Stmt
stmt =
  (do void $ matchKeyword "for"
      forSt) <|>
  (do void $ matchKeyword "while"
      whileSt) <|>
  (do void $ matchKeyword "if"
      ifSt) <|>
  (do var <- iden
      assignSt var) <|>
  (blockSt <?> "statement")

startParser :: String -> Either ParseError Stmt
startParser programText = parse stmt "parameter" (alexScanTokens programText)

-- Tests
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
