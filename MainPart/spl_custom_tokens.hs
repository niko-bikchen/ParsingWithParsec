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
  | SymbolToken
      { lin, col :: Int
      , customSymbol :: String
      }

instance Show DataToken where
  show ArithmOpToken {arithmOperator = op} = "ArithmOpToken " ++ show op
  show BoolOpToken {boolOperator = op} = "BoolOpToken " ++ show op
  show SymbolToken {customSymbol = sym} = "SymbolToken " ++ show sym
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
  (==) SymbolToken {customSymbol = sym1} SymbolToken {customSymbol = sym2} =
    sym1 == sym2
  (==) KeywordToken {reservedWord = rw1} KeywordToken {reservedWord = rw2} =
    rw1 == rw2
  (==) VarTypeToken {varType = vt1} VarTypeToken {varType = vt2} = vt1 == vt2
  (==) VarNameToken {varName = vn1} VarNameToken {varName = vn2} = vn1 == vn2
  (==) AssignOpToken {operator = op1} AssignOpToken {operator = op2} =
    op1 == op2
  (==) NumConstToken {numValue = val1} NumConstToken {numValue = val2} =
    val1 == val2
  (==) BoolConstToken {boolValue = val1} BoolConstToken {boolValue = val2} =
    val1 == val2

boolOperators :: [String]
boolOperators = ["&", "|", "==", "<=", ">=", "<", ">"]

arithmeticOperators :: [String]
arithmeticOperators = ["/", "*", "-", "++", "+"]

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

parseXOp :: Int -> Int -> String -> [String] -> Maybe (String, Int, Int, String)
parseXOp lin col str operators =
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
  case parseXOp lin col str arithmeticOperators of
    Just (op, newLin, newCol, rest) ->
      Just (ArithmOpToken lin col op, newLin, newCol, rest)
    Nothing -> Nothing

parseBoolOp :: Int -> Int -> String -> Maybe (DataToken, Int, Int, String)
parseBoolOp lin col str =
  case parseXOp lin col str boolOperators of
    Just (op, newLin, newCol, rest) ->
      Just (BoolOpToken lin col op, newLin, newCol, rest)
    Nothing -> Nothing

-- TODO Check whether the token indexing is correct
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
  | any (`isPrefixOf` input) ["true", "false"] =
    case parseBoolLiteral lin col input of
      Just (tok, newLin, newCol, rest) -> tok : lexer newLin newCol rest
      Nothing -> lexer lin col input
  | isDigit chr =
    case parseNumLiteral lin col input of
      Just (tok, newLin, newCol, rest) -> tok : lexer newLin newCol rest
      Nothing -> lexer lin col input
  | "\n" `isPrefixOf` input || "\r\n" `isPrefixOf` input =
    case stripPrefix "\n" input of
      Just rest -> lexer (lin + 1) (col + 1) rest
      Nothing ->
        case stripPrefix "\r\n" input of
          Just rest -> lexer (lin + 1) (col + 2) rest
          Nothing -> lexer lin col input
  | isSpace chr = lexer lin (col + 1) chrs
