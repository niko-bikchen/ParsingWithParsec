import Control.Monad
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

type Program = Stmt

{- Task� 
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
whitespaces :: Parsec.Parsec String [(String, SourcePos)] ()
whitespaces = void $ many $ oneOf " \n\t"

lexem ::
     Parsec.Parsec String [(String, SourcePos)] a
  -> Parsec.Parsec String [(String, SourcePos)] a
lexem parser = do
  res <- parser
  whitespaces
  return res

symbol :: Char -> Parsec.Parsec String [(String, SourcePos)] ()
symbol chr = void $ lexem $ char chr

keyword :: String -> Parsec.Parsec String [(String, SourcePos)] ()
keyword str = try $ lexem $ string str >> notFollowedBy alphaNum

oprtr :: String -> Bop -> Parsec.Parsec String [(String, SourcePos)] Bop
oprtr str bop = do
  void $ string str
  whitespaces
  return bop

exprOp ::
     Parsec.Parsec String [(String, SourcePos)] Bop
  -> Parsec.Parsec String [(String, SourcePos)] (Exp -> Exp -> Exp)
exprOp p = do
  x <- lexem p
  return (flip Op x)

number :: Parsec.Parsec String [(String, SourcePos)] Int
number = do
  s <- string "-" <|> return []
  cs <- many1 digit
  return $ read (s ++ cs)

idenName :: Parsec.Parsec String [(String, SourcePos)] String
idenName = do
  whitespaces
  a <- letter
  cs <- many (letter <|> digit)
  return (a : cs)

iden :: Parsec.Parsec String [(String, SourcePos)] String
iden =
  try $ do
    name <- idenName
    if name `elem`
       ["int", "bool", "if", "while", "for", "else", "True", "False"]
      then unexpected ("use of reserved word " ++ show name)
      else return name

idenType :: Parsec.Parsec String [(String, SourcePos)] Type
idenType =
  (do keyword "int"
      return It) <|>
  (do keyword "bool"
      return Bt)

mulOrDivOp :: Parsec.Parsec String [(String, SourcePos)] Bop
mulOrDivOp = oprtr "*" Times <|> oprtr "/" Div

addOrSubOp :: Parsec.Parsec String [(String, SourcePos)] Bop
addOrSubOp = try (oprtr "+" Plus) <|> try (oprtr "-" Minus)

andOp :: Parsec.Parsec String [(String, SourcePos)] Bop
andOp = oprtr "&" Ba

orOp :: Parsec.Parsec String [(String, SourcePos)] Bop
orOp = oprtr "|" Bo

relOp :: Parsec.Parsec String [(String, SourcePos)] Bop
relOp =
  try (oprtr ">=" Ge) <|> try (oprtr ">" Gt) <|> try (oprtr "<=" Le) <|>
  try (oprtr "==" Eql) <|>
  try (oprtr "<" Lt)

{-
  factor = '(' expr ')' | number | "true" | "false" | iden
  term   = factor { mulOrDivOp factor }
  relat  = term { addOrSubOp term }
  conj   = relat [relOp relat]
  disj   = conj { orOp conj}
  expr   = disj { andOp disj}
-}
bopPrsr ::
     Parsec.Parsec String [(String, SourcePos)] Exp
  -> Parsec.Parsec String [(String, SourcePos)] Bop
  -> Parsec.Parsec String [(String, SourcePos)] Exp
bopPrsr prsr bin = do
  x <- prsr
  rest x
  where
    rest x =
      (do b <- bin
          y <- prsr
          rest $ Op x b y) <|>
      return x

factor :: Parsec.Parsec String [(String, SourcePos)] Exp
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

term :: Parsec.Parsec String [(String, SourcePos)] Exp
term = bopPrsr factor mulOrDivOp

relat :: Parsec.Parsec String [(String, SourcePos)] Exp
relat = bopPrsr term addOrSubOp

conj :: Parsec.Parsec String [(String, SourcePos)] Exp
conj = bopPrsr relat relOp

disj :: Parsec.Parsec String [(String, SourcePos)] Exp
disj = bopPrsr conj orOp

expr :: Parsec.Parsec String [(String, SourcePos)] Exp
expr = bopPrsr disj andOp

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
stmt :: Parsec.Parsec String [(String, SourcePos)] Stmt
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

forSt :: Parsec.Parsec String [(String, SourcePos)] Stmt
forSt = do
  symbol '('
  st1 <- stmt
  symbol ';'
  ex <- expr
  symbol ';'
  st2 <- stmt
  symbol ')'
  For st1 ex st2 <$> stmt

whileSt :: Parsec.Parsec String [(String, SourcePos)] Stmt
whileSt = do
  symbol '('
  ex <- expr
  symbol ')'
  While ex <$> stmt

ifSt :: Parsec.Parsec String [(String, SourcePos)] Stmt
ifSt = do
  symbol '('
  ex <- expr
  symbol ')'
  st1 <- stmt
  string "else"
  If ex st1 <$> stmt

assignSt :: String -> Parsec.Parsec String [(String, SourcePos)] Stmt
assignSt varName =
  (do string "++"
      return $ Incr varName) <|>
  (do string ":="
      whitespaces
      Assign varName <$> expr)

defin :: Parsec.Parsec String [(String, SourcePos)] (String, Type)
defin = do
  pos <- Parsec.getPosition
  tp <- idenType
  id <- idenName
  Parsec.modifyState (\state -> state ++ [(id, pos)])
  symbol ';'
  return (id, tp)

listStmt :: Parsec.Parsec String [(String, SourcePos)] [Stmt]
listStmt = stmt `sepBy` (symbol ';')

blockSt :: Parsec.Parsec String [(String, SourcePos)] Stmt
blockSt = do
  symbol '{'
  defins <- many defin
  stmts <- listStmt
  symbol '}'
  return $ Block defins stmts

program :: Parsec.Parsec String [(String, SourcePos)] Stmt
program = do
  spaces
  r <- stmt
  eof
  return r

parseSPL =
  Parsec.runParser
    (do pr <- program
        st <- Parsec.getState
        return (pr, st))
    []
    ""

parseFile :: String -> IO Stmt
parseFile file = do
  program_txt <- readFile file
  case Parsec.runParser program [] "" program_txt of
    Left e -> print e >> fail "parse error"
    Right r -> return r

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
