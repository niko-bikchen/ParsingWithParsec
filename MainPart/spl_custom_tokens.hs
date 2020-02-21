import Control.Monad
import Data.Char
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
      , arithmOperation :: String
      }
  | BoolOpToken
      { lin, col :: Int
      , boolOperation :: String
      }
  | SymbolToken
      { lin, col :: Int
      , customSymbol :: String
      }
  | KeywordToken
      { lin, col :: Int
      , reservedWord :: String
      }
  | OpToken
      { lin, col :: Int
      , operator :: String
      }
  | VarNameToken
      { lin, col :: Int
      , varName :: String
      }

instance Show DataToken where
  show ArithmOpToken {arithmOperation = op} = "ArithmOpToken " ++ show op
  show BoolOpToken {boolOperation = op} = "BoolOpToken " ++ show op
  show SymbolToken {customSymbol = sym} = "SymbolToken " ++ show sym
  show KeywordToken {reservedWord = rw} = "KeywordToken " ++ show rw
  show VarNameToken {varName = vn} = "VarNameToken " ++ show vn

instance Eq DataToken where
  (==) ArithmOpToken {arithmOperation = ao1} ArithmOpToken {arithmOperation = ao2} =
    ao1 == ao2
  (==) BoolOpToken {boolOperation = bo1} BoolOpToken {boolOperation = bo2} =
    bo1 == bo2
  (==) SymbolToken {customSymbol = sym1} SymbolToken {customSymbol = sym2} =
    sym1 == sym2
  (==) KeywordToken {reservedWord = rw1} KeywordToken {reservedWord = rw2} =
    rw1 == rw2
  (==) VarNameToken {varName = vn1} VarNameToken {varName = vn2} = vn1 == vn2

lexer :: Int -> Int -> String -> [DataToken]
lexer _ _ [] = []
lexer lin col ('+':rest) = ArithmOpToken lin col "+" : lexer lin (col + 1) rest
lexer lin col ('-':rest) = ArithmOpToken lin col "-" : lexer lin (col + 1) rest
lexer lin col ('*':rest) = ArithmOpToken lin col "*" : lexer lin (col + 1) rest
lexer lin col ('/':rest) = ArithmOpToken lin col "/" : lexer lin (col + 1) rest
lexer lin col ('+':'+':rest) =
  ArithmOpToken lin col "++" : lexer lin (col + 1) rest
