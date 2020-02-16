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

-- instance Show Token is needed for eof so it can produce error messages
example, wrong, bad :: [Token]
example = [N 1 3 111, Plus 1 10, N 2 1 222]

wrong = [N 1 3 111, Plus 1 10, N 2 1 222, N 2 5 333]

bad = [N 1 3 111, Plus 1 10, N 2 1 222, Plus 2 5]

play :: [Token] -> Either ParseError Integer
play s = parse pmain "parameter" s

-- type Parsec s u a = ParsecT s u Identity a
pmain :: Parsec [Token] () Integer
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

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) =
  setSourceLine (setSourceColumn pos (col tok)) (lin tok)
update_pos pos _ [] = pos
-- setSourceLine :: SourcePos -> Line -> SourcePos
-- ###############################################
-- Receives SourcePos and Line (Int) number
-- and creates new SourcePos with the given Line
-- ###############################################
-- setSourceColumn works similar to setSourceLine
