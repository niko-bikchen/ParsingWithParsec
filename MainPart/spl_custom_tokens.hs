{-
Name: spl_custom_tokens.hs
Author: Nikolay Bikchentaev
Version: 1.0
===========================
This program parses SPL source code
by decomposing it into custom tokens
which we define below instead of
standard Parsec tokens represented as Char.
We parse these custom tokens with the
functions we define ourselves, since
the standard Parsec functions are only
for Char tokens
-}
{-# OPTIONS_GHC -Wall #-}

module SPLCustom where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
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

-- Custom token definition
data DataToken
  = ArithmOpToken
      { line, column :: Int
      , arithmOperator :: String
      }
  | BoolOpToken
      { line, column :: Int
      , boolOperator :: String
      }
  | AssignOpToken
      { line, column :: Int
      , operator :: String
      }
  | KeywordToken
      { line, column :: Int
      , reservedWord :: String
      }
  | VarTypeToken
      { line, column :: Int
      , varType :: String
      }
  | VarNameToken
      { line, column :: Int
      , varName :: String
      }
  | NumConstToken
      { line, column :: Int
      , numValue :: Int
      }
  | BoolConstToken
      { line, column :: Int
      , boolValue :: Bool
      }
  | DelimiterSymbolToken
      { line, column :: Int
      , customSymbol :: String
      }

instance Show DataToken where
  show ArithmOpToken {arithmOperator = op} = "ArithmOpToken " ++ show op
  show BoolOpToken {boolOperator = op} = "BoolOpToken " ++ show op
  show DelimiterSymbolToken {customSymbol = sym} =
    "DelimiterSymbolToken " ++ show sym
  show KeywordToken {reservedWord = rw} = "KeywordToken " ++ show rw
  show VarNameToken {varName = vn} = "VarNameToken " ++ show vn
  show VarTypeToken {varType = vt} = "VarTypeToken " ++ show vt
  show AssignOpToken {operator = op} = "AssignOpToken " ++ show op
  show NumConstToken {numValue = val} = "NumConstToken " ++ show val
  show BoolConstToken {boolValue = val} = "BoolConstToken " ++ show val

instance Eq DataToken where
  (==) ArithmOpToken {arithmOperator = ao1} ArithmOpToken {arithmOperator = ao2} =
    ao1 == ao2
  (==) BoolOpToken {boolOperator = bo1} BoolOpToken {boolOperator = bo2} =
    bo1 == bo2
  (==) DelimiterSymbolToken {customSymbol = sym1} DelimiterSymbolToken {customSymbol = sym2} =
    sym1 == sym2
  (==) KeywordToken {reservedWord = rw1} KeywordToken {reservedWord = rw2} =
    rw1 == rw2
  (==) VarTypeToken {varType = vt1} VarTypeToken {varType = vt2} = vt1 == vt2
  (==) VarNameToken {} VarNameToken {} = True
  (==) AssignOpToken {operator = op1} AssignOpToken {operator = op2} =
    op1 == op2
  (==) NumConstToken {} NumConstToken {} = True
  (==) BoolConstToken {boolValue = val1} BoolConstToken {boolValue = val2} =
    val1 == val2
  (==) _ _ = False

boolOperators :: [String]
boolOperators = ["&", "|", "==", "<=", ">=", "<", ">"]

arithmeticOperators :: [String]
arithmeticOperators = ["/", "*", "-", "++", "+"]

varTypes :: [String]
varTypes = ["bool", "int"]

reservedWords :: [String]
reservedWords = ["if", "else", "while", "for"]

delimitingSymbols :: [String]
delimitingSymbols = [";", "(", ")", "{", "}"]

parseNumLiteral :: Int -> Int -> String -> Maybe (DataToken, Int, Int, String)
parseNumLiteral lin col str =
  case span isDigit str of
    ([], _) -> Nothing
    (strNum, rest) ->
      Just
        ( NumConstToken lin col (read strNum :: Int)
        , lin
        , col + length strNum
        , rest)

parseVarName :: Int -> Int -> String -> Maybe (DataToken, Int, Int, String)
parseVarName lin col str =
  case span isAlphaNum str of
    ([], _) -> Nothing
    (name, rest) ->
      Just (VarNameToken lin col name, lin, col + length name, rest)

parseBoolLiteral :: Int -> Int -> String -> Maybe (DataToken, Int, Int, String)
parseBoolLiteral lin col str =
  case constIndex of
    Just index ->
      let boolStrConst = (boolStrConsts !! index)
       in Just
            ( BoolConstToken lin col (boolConsts !! index)
            , lin
            , col + length boolStrConst
            , fromJust (stripPrefix boolStrConst str))
    Nothing -> Nothing
  where
    boolConsts = [True, False]
    boolStrConsts = ["true", "false"]
    presentStrConst = map (`isPrefixOf` str) boolStrConsts
    constIndex = elemIndex True presentStrConst

parsePrefix ::
     Int -> Int -> String -> [String] -> Maybe (String, Int, Int, String)
parsePrefix lin col str operators =
  case oprtrtIndex of
    Just index ->
      let oprtrt = (xOperators !! index)
       in Just
            ( oprtrt
            , lin
            , col + length oprtrt
            , fromJust (stripPrefix oprtrt str))
    Nothing -> Nothing
  where
    xOperators = operators
    presentOperator = map (`isPrefixOf` str) xOperators
    oprtrtIndex = elemIndex True presentOperator

parseArithmOp :: Int -> Int -> String -> Maybe (DataToken, Int, Int, String)
parseArithmOp lin col str =
  case parsePrefix lin col str arithmeticOperators of
    Just (op, newLin, newCol, rest) ->
      Just (ArithmOpToken lin col op, newLin, newCol, rest)
    Nothing -> Nothing

parseBoolOp :: Int -> Int -> String -> Maybe (DataToken, Int, Int, String)
parseBoolOp lin col str =
  case parsePrefix lin col str boolOperators of
    Just (op, newLin, newCol, rest) ->
      Just (BoolOpToken lin col op, newLin, newCol, rest)
    Nothing -> Nothing

parseVarType :: Int -> Int -> String -> Maybe (DataToken, Int, Int, String)
parseVarType lin col str =
  case parsePrefix lin col str varTypes of
    Just (op, newLin, newCol, rest) ->
      Just (VarTypeToken lin col op, newLin, newCol, rest)
    Nothing -> Nothing

parseReservedWord :: Int -> Int -> String -> Maybe (DataToken, Int, Int, String)
parseReservedWord lin col str =
  case parsePrefix lin col str reservedWords of
    Just (op, newLin, newCol, rest) ->
      Just (KeywordToken lin col op, newLin, newCol, rest)
    Nothing -> Nothing

parseDelimiterSymbol ::
     Int -> Int -> String -> Maybe (DataToken, Int, Int, String)
parseDelimiterSymbol lin col str =
  case parsePrefix lin col str delimitingSymbols of
    Just (op, newLin, newCol, rest) ->
      Just (DelimiterSymbolToken lin col op, newLin, newCol, rest)
    Nothing -> Nothing

parseAssignmentOp :: Int -> Int -> String -> Maybe (DataToken, Int, Int, String)
parseAssignmentOp lin col str =
  case parsePrefix lin col str [":="] of
    Just (op, newLin, newCol, rest) ->
      Just (AssignOpToken lin col op, newLin, newCol, rest)
    Nothing -> Nothing

-- Decomposes input source code into tokens
lexer :: Int -> Int -> String -> [DataToken]
lexer _ _ [] = []
lexer lin col input@(ch:chrs)
  | any (`isPrefixOf` input) arithmeticOperators =
    case parseArithmOp lin col input of
      Just (tok, newLin, newCol, rest) -> tok : lexer newLin newCol rest
      Nothing -> lexer lin col input
  | any (`isPrefixOf` input) boolOperators =
    case parseBoolOp lin col input of
      Just (tok, newLin, newCol, rest) -> tok : lexer newLin newCol rest
      Nothing -> lexer lin col input
  | any (`isPrefixOf` input) [":="] =
    case parseAssignmentOp lin col input of
      Just (tok, newLin, newCol, rest) -> tok : lexer newLin newCol rest
      Nothing -> lexer lin col input
  | any (`isPrefixOf` input) ["true", "false"] =
    case parseBoolLiteral lin col input of
      Just (tok, newLin, newCol, rest) -> tok : lexer newLin newCol rest
      Nothing -> lexer lin col input
  | any (`isPrefixOf` input) varTypes =
    case parseVarType lin col input of
      Just (tok, newLin, newCol, rest) -> tok : lexer newLin newCol rest
      Nothing -> lexer lin col input
  | any (`isPrefixOf` input) reservedWords =
    case parseReservedWord lin col input of
      Just (tok, newLin, newCol, rest) -> tok : lexer newLin newCol rest
      Nothing -> lexer lin col input
  | any (`isPrefixOf` input) delimitingSymbols =
    case parseDelimiterSymbol lin col input of
      Just (tok, newLin, newCol, rest) -> tok : lexer newLin newCol rest
      Nothing -> lexer lin col input
  | isDigit ch =
    case parseNumLiteral lin col input of
      Just (tok, newLin, newCol, rest) -> tok : lexer newLin newCol rest
      Nothing -> lexer lin col input
  | "\n" `isPrefixOf` input || "\r\n" `isPrefixOf` input =
    case stripPrefix "\n" input of
      Just rest -> lexer (lin + 1) 1 rest
      Nothing ->
        case stripPrefix "\r\n" input of
          Just rest -> lexer (lin + 1) (col + 2) rest
          Nothing -> lexer lin col input
  | isSpace ch = lexer lin (col + 1) chrs
  | isAlpha ch =
    case parseVarName lin col input of
      Just (tok, newLin, newCol, rest) -> tok : lexer newLin newCol rest
      Nothing -> lexer lin col input
  | otherwise =
    error
      ("lexer: unexpected character: " ++
       show ch ++ " at line " ++ show lin ++ " column " ++ show col)

runLexer :: String -> [DataToken]
runLexer = lexer 1 1

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
startParser programText = parse stmt "parameter" (runLexer programText)

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
