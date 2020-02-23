import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.IO
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

data DataToken
  = ArithmOpToken
      { lin, col :: Int
      , arithmOperator :: String
      }
  | BoolOpToken
      { lin, col :: Int
      , boolOperator :: String
      }
  | AssignOpToken
      { lin, col :: Int
      , operator :: String
      }
  | KeywordToken
      { lin, col :: Int
      , reservedWord :: String
      }
  | VarTypeToken
      { lin, col :: Int
      , varType :: String
      }
  | VarNameToken
      { lin, col :: Int
      , varName :: String
      }
  | NumConstToken
      { lin, col :: Int
      , numValue :: Int
      }
  | BoolConstToken
      { lin, col :: Int
      , boolValue :: Bool
      }
  | DelimiterSymbolToken
      { lin, col :: Int
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

boolOperators :: [String]
boolOperators = ["&", "|", "==", "<=", ">=", "<", ">"]

arithmeticOperators :: [String]
arithmeticOperators = ["/", "*", "-", "++", "+"]

varTypes :: [String]
varTypes = ["bool", "int"]

reservedWords :: [String]
reservedWords = ["if", "else", "while", "for", "then"]

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

lexer :: Int -> Int -> String -> [DataToken]
lexer _ _ [] = []
lexer lin col input@(chr:chrs)
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
  | isDigit chr =
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
  | isSpace chr = lexer lin (col + 1) chrs
  | isAlpha chr =
    case parseVarName lin col input of
      Just (tok, newLin, newCol, rest) -> tok : lexer newLin newCol rest
      Nothing -> lexer lin col input
  | otherwise =
    error
      ("lexer: unexpected character: " ++
       show chr ++ " at line " ++ show lin ++ " column " ++ show col)

runLexer :: String -> [DataToken]
runLexer = lexer 1 1

updatePos :: SourcePos -> DataToken -> [DataToken] -> SourcePos
updatePos pos _ (tok:_) =
  setSourceLine (setSourceColumn pos (col tok)) (lin tok)
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

matchBoolOp :: String -> Parsec.Parsec [DataToken] () DataToken
matchBoolOp op = satisfyT (== BoolOpToken 0 0 op)

matchBoolConst :: Bool -> Parsec.Parsec [DataToken] () DataToken
matchBoolConst const = satisfyT (== BoolConstToken 0 0 const)

matchNumber :: Parsec.Parsec [DataToken] () DataToken
matchNumber = satisfyT (== NumConstToken 0 0 0)

matchVarName :: Parsec.Parsec [DataToken] () DataToken
matchVarName = satisfyT (== VarNameToken 0 0 "")

matchVarType :: String -> Parsec.Parsec [DataToken] () DataToken
matchVarType varType = satisfyT (== VarTypeToken 0 0 varType)
-- commenceParsing :: String -> Either ParseError Stmt
-- commenceParsing programText = parse main "parameter" programText
