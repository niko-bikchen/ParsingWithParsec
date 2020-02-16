{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

import qualified Data.Vector as V
import Data.Vector (Vector)
import Text.Parsec

data Token
  = N
      { lin, col :: Int
      , val :: Integer
      }
  | Plus
      { lin, col :: Int
      }

instance Show Token where
  show N {val = x} = "number " ++ show x
  show Plus {} = "plus"

-- Show Token is needed for eof so it can produce error messages
example, wrong, bad :: Vector Token
example = V.fromList [N 1 3 111, Plus 1 10, N 2 1 222]

wrong = V.fromList [N 1 3 111, Plus 1 10, N 2 1 222, N 2 5 333]

bad = V.fromList [N 1 3 111, Plus 1 10, N 2 1 222, Plus 2 5]

play :: Vector Token -> Either ParseError Integer
play s = parse pmain "parameter" s

-- type Parsec s u = ParsecT s u Identity
pmain :: Parsec (Vector Token) () Integer
pmain = do
  x <- pnum `chainl1` pplus
  eof
  return x

pnum = tokenPrim show update_pos get_num
  where
    get_num N {val = x} = Just x
    get_num _ = Nothing

pplus = tokenPrim show update_pos is_plus
  where
    is_plus Plus {} = Just (+)
    is_plus _ = Nothing

update_pos :: SourcePos -> Token -> Vector Token -> SourcePos
update_pos pos _ v
  | V.null v = pos -- for simplicity; there is a better answer
  | otherwise = setSourceLine (setSourceColumn pos (col tok)) (lin tok)
  where
    tok = V.head v

instance (Monad m) => Stream (Vector a) m a where
  uncons v
    | V.null v = return Nothing
    | otherwise = return (Just (V.head v, V.tail v))
