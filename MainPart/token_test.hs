import Control.Monad
import Data.Char
import System.IO
import qualified Text.Parsec as Parsec
import Text.ParserCombinators.Parsec

type IndexedString = [(Char, (Int, Int))]

data ParsecToken
  = PlusToken
      { lin, col :: Int
      }
  | TimesToken
      { lin, col :: Int
      }
  | OpenToken
      { lin, col :: Int
      }
  | CloseToken
      { lin, col :: Int
      }
  | IntToken
      { lin, col :: Int
      , val :: Int
      }

plus :: ParsecToken
plus = PlusToken 0 0

times :: ParsecToken
times = TimesToken 0 0

openBracket :: ParsecToken
openBracket = OpenToken 0 0

closeBracket :: ParsecToken
closeBracket = CloseToken 0 0

number :: ParsecToken
number = IntToken 0 0 0

instance Show ParsecToken where
  show PlusToken {} = "PlusTok"
  show TimesToken {} = "MultiplyTok"
  show OpenToken {} = "OpenBracketTok"
  show CloseToken {} = "CloseBracketTok"
  show IntToken {val = x} = "DigitTok " ++ show x

instance Eq ParsecToken where
  (==) PlusToken {} PlusToken {} = True
  (==) TimesToken {} TimesToken {} = True
  (==) OpenToken {} OpenToken {} = True
  (==) CloseToken {} CloseToken {} = True
  (==) IntToken {} IntToken {} = True
  (==) _ _ = False

data Exp
  = IntLit Int
  | Add Exp Exp
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

data Value
  = I Int
  | B Bool
  deriving (Show, Eq)

indexLines :: String -> IndexedString
indexLines str = indexChars $ zip (lines str) [1 ..]

indexChars :: [(String, Int)] -> IndexedString
indexChars [] = []
indexChars ((line, lineId):restLines) =
  map (\(chr, chrId) -> (chr, (lineId, chrId))) (zip line [1 ..]) ++
  indexChars restLines

parseFullInt ::
     IndexedString -> (String, IndexedString) -> (String, IndexedString)
parseFullInt [] res = res
parseFullInt (sym@(chr, _):rest) (g, b)
  | isDigit chr = parseFullInt rest (g ++ [chr], b)
  | otherwise = (g, sym : rest)

lexer :: IndexedString -> [ParsecToken]
lexer [] = []
lexer (('+', (lin, col)):rest) = PlusToken {lin = lin, col = col} : lexer rest
lexer (('*', (lin, col)):rest) = TimesToken {lin = lin, col = col} : lexer rest
lexer (('(', (lin, col)):rest) = OpenToken {lin = lin, col = col} : lexer rest
lexer ((')', (lin, col)):rest) = CloseToken {lin = lin, col = col} : lexer rest
lexer ((chr, (lin, col)):rest)
  | isSpace chr = lexer rest
lexer str@((chr, (lin, col)):_)
  | isDigit chr =
    IntToken {val = (stringToInt digitStr), lin = lin, col = col} : lexer rest
  where
    (digitStr, rest) = parseFullInt str ([], [])
    stringToInt = foldl (\acc chr -> 10 * acc + digitToInt chr) 0
lexer (chr:_) = error ("lexer: unexpected character: '" ++ show chr ++ "'")

satisfyT ::
     (Monad m)
  => (ParsecToken -> Bool)
  -> Parsec.ParsecT [ParsecToken] u m ParsecToken
satisfyT f =
  tokenPrim
    show
    updatePos
    (\c ->
       if f c
         then Just c
         else Nothing)

updatePos :: SourcePos -> ParsecToken -> [ParsecToken] -> SourcePos
updatePos pos _ (tok:_) =
  setSourceLine (setSourceColumn pos (col tok)) (lin tok)
updatePos pos _ [] = pos

digitTok :: Parsec.Parsec [ParsecToken] () ParsecToken
digitTok = satisfyT (== number)

symbolTok :: ParsecToken -> Parsec.Parsec [ParsecToken] () ParsecToken
symbolTok tok = satisfyT (== tok)

oprtr :: ParsecToken -> Bop -> Parsec.Parsec [ParsecToken] () Bop
oprtr tok bop = do
  void $ symbolTok tok
  return bop

addOrMul :: Parsec.Parsec [ParsecToken] () Bop
addOrMul = try (oprtr plus Plus) <|> try (oprtr times Times)

bopPrsr ::
     Parsec.Parsec [ParsecToken] () Exp
  -> Parsec.Parsec [ParsecToken] () Bop
  -> Parsec.Parsec [ParsecToken] () Exp
bopPrsr prsr bin = do
  x <- prsr
  rest x
  where
    rest x =
      (do b <- bin
          y <- prsr
          rest $ Op x b y) <|>
      return x

term :: Parsec.Parsec [ParsecToken] () Exp
term = bopPrsr factor addOrMul

factor :: Parsec.Parsec [ParsecToken] () Exp
factor =
  (do symbolTok openBracket
      x <- term
      symbolTok closeBracket
      return x) <|>
  (IntLit . val <$> digitTok)
