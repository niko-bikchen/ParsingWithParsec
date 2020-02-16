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
  = PlusToken
      { lin, col :: Int
      }
  | MinusToken
      { lin, col :: Int
      }
  | MultiplyToken
      { lin, col :: Int
      }
  | DivideToken
      { lin, col :: Int
      }
  | BoolAndToken
      { lin, col :: Int
      }
  | BoolOrToken
      { lin, col :: Int
      }
  | BoolEqToken
      { lin, col :: Int
      }
  | BoolLtToken
      { lin, col :: Int
      }
  | BoolGtToken
      { lin, col :: Int
      }
  | BoolLeToken
      { lin, col :: Int
      }
  | BoolGeToken
      { lin, col :: Int
      }
