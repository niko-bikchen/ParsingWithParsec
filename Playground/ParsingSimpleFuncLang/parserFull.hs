import Text.Parsec hiding (Empty)
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

data Expr
  = CInt Integer
  | CBool Bool
  | CVar String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Equal Expr Expr
  | Less Expr Expr
  | LessEq Expr Expr
  | Great Expr Expr
  | GreatEq Expr Expr
  | Empty -- []
  | Cons Expr Expr
  | If Expr Expr Expr
  | Function String Expr
  | Appl Expr Expr
  | Let String [String] Expr Expr -- Let String String String ... = Expr In Expr
  | Semi Expr Expr -- Expr; Expr
  | Case Expr Expr String String Expr -- Case Expr Of [] -> Expr | (String, String) -> Exp
  deriving (Show)

names = words "True False Function If Then Else Let In Case Of" -- reserved names

opNames = words "-> && || ! + - * / % = ; < <= > >= :" -- reserved operations

lexerConfig =
  emptyDef
    { Token.commentStart = "/*" -- adding comments is easy
    , Token.commentEnd = "*/"
    , Token.commentLine = "#"
    , Token.identStart = letter -- identifiers must start with a letter
    , Token.identLetter = alphaNum <|> char '_' <|> char '\''
    , Token.reservedNames = names
    , Token.reservedOpNames = opNames
    }

lexer = Token.makeTokenParser lexerConfig

identifier = Token.identifier lexer -- parses a valid identifier in our language

symbol = Token.symbol lexer -- parses a symbol like "]"

reserved = Token.reserved lexer -- parses a reserved word like "If"

reservedOp = Token.reservedOp lexer -- parses a reserved operation like "<="

parens = Token.parens lexer -- parses parenthesis surrounding the parser passed to i

brackets = Token.brackets lexer -- parses brackets surrounding the parser passed to it

commaSep = Token.commaSep lexer -- parses some or no comma separated instances of

-- the argument parser
integer = Token.integer lexer -- parses an integer

whiteSpace = Token.whiteSpace lexer -- parses whitespace

binary name label assoc =
  Infix
    (do reservedOp name
        return (\x y -> label x y))
    assoc

prefix name label = Prefix (reservedOp name *> return (\x -> label x))

opTable =
  [ [prefix "!" Not]
  , [appl]
  , [ binary "*" Mul AssocLeft
    , binary "/" Div AssocLeft
    , binary "%" Mod AssocLeft
    ]
  , [binary "+" Add AssocLeft, binary "-" Sub AssocLeft]
  , [ binary "=" Equal AssocLeft
    , binary "<" Less AssocLeft
    , binary "<=" LessEq AssocLeft
    , binary ">" Great AssocLeft
    , binary ">=" GreatEq AssocLeft
    ]
  , [binary "&&" And AssocLeft]
  , [binary "||" Or AssocLeft]
  , [binary ":" Cons AssocRight]
  , [binary ";" Semi AssocLeft]
  ]

appl = Infix space AssocLeft
  where
    space =
      whiteSpace *> notFollowedBy (choice . map reservedOp $ opNames) *>
      return (\x y -> Appl x y)

cint :: Parser Expr
cint = CInt <$> integer

cbool :: Parser Expr
cbool = CBool True <$ reserved "True" <|> CBool False <$ reserved "False"

cvar :: Parser Expr
cvar = CVar <$> identifier

list :: Parser Expr
list = toCons <$> brackets (commaSep expr)
  where
    toCons [] = Empty
    toCons (x:xs) = Cons x (toCons xs)

term :: Parser Expr
term =
  cint <|> cbool <|> cvar <|> list <|>
  parens expr -- parentheses surrounded expression

opExpr :: Parser Expr
opExpr = buildExpressionParser opTable term

letExpr :: Parser Expr
letExpr =
  reserved "Let" *> -- parse the reserved word Let; return the do block
   do
    s <- sepBy1 identifier whiteSpace
    reservedOp "="
    e <- expr
    reserved "In"
    e' <- expr
    case s of
      (x:xs) -> return $ Let x xs e e' -- we must have at least one

caseExpr :: Parser Expr
caseExpr =
  reserved "Case" *> do
    p <- expr
    reserved "Of" *> symbol "[]" *> reservedOp "->" -- parse an "Of", a "[]", then a "->"
    x <- expr
    reservedOp "|"
    (s, t) <-
      parens $ do
        s' <- identifier -- return the do block within parentheses
        reservedOp ":"
        t' <- identifier
        return (s', t')
    reservedOp "->"
    y <- expr
    return $ Case p x s t y

function :: Parser Expr
function =
  reserved "Function" *>
  ((\x y -> Function x y) <$> identifier <*> (reservedOp "->" *> expr))

ifExpr :: Parser Expr
ifExpr =
  reserved "If" *>
  ((\x y z -> If x y z) <$> expr <*> (reserved "Then" *> expr) <*>
   (reserved "Else" *> expr))

expr :: Parser Expr
expr = function <|> letExpr <|> ifExpr <|> caseExpr <|> opExpr <|> term

parseString :: Parser Expr -> String -> Either ParseError Expr
parseString e s = parse (e <* eof) "" s

parseFile :: Parser Expr -> FilePath -> IO (Either ParseError Expr)
parseFile e f = parseFromFile (e <* eof) f
