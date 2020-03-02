{
module AlexLexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$white = [\ \t \n]

tokens :-

$white+                            ;
"\r\n"                             ;
[\+ \- \* \/]|"++"                 { \(AlexPn _ lin col) op -> ArithmOpToken lin col op }
[\| \& \< \>]|"=="|"<="|">="       { \(AlexPn _ lin col) op -> BoolOpToken lin col op }
":="                               { \(AlexPn _ lin col) op -> AssignOpToken lin col op }
"true"                             { \(AlexPn _ lin col) _ -> BoolConstToken lin col True }
"false"                            { \(AlexPn _ lin col) _ -> BoolConstToken lin col False }
"bool"|"int"                       { \(AlexPn _ lin col) vType -> VarTypeToken lin col vType }
"if"|"else"|"while"|"for"          { \(AlexPn _ lin col) kWord -> KeywordToken lin col kWord }
[\( \) \{ \} \;]                   { \(AlexPn _ lin col) delimSym -> DelimiterSymbolToken lin col delimSym }
$digit+                            { \(AlexPn _ lin col) nConst -> NumConstToken lin col (read nConst) }
$alpha[$alpha $digit \_ \']*       { \(AlexPn _ lin col) vName -> VarNameToken lin col vName }

{
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
}