module BC.Parser (
    Expr(..),
    pExpr,
    evalConstExpr
) where

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Bits

type Parser = Parsec Void String

-- Essentially an RValue, will need to be checked for LValues in
-- assignment, increment, decrement, address at a later stage
data Expr
    = Var String -- ^ Variable name

    -- Basic primitive types
    | IntT Int
    | FloatT Double
    | StringT String
    
    -- Unary primitive operations
    | Neg Expr
    | Addr Expr
    | Deref Expr
    | IncL Expr
    | IncR Expr
    | DecL Expr
    | DecR Expr
    | Not Expr -- Takes an integer, returns 1 if zero and 0 in all other cases

    -- Binary primitive operations
    -- Note that there are no short circuiters (||, &&, etc) and no bitwise xor or not here!
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Mod Expr Expr
    | Gt  Expr Expr
    | Ge  Expr Expr
    | Lt  Expr Expr
    | Le  Expr Expr
    | Eq  Expr Expr
    | NotEq Expr Expr
    | BitOr Expr Expr
    | BitAnd Expr Expr
    | ShiftL Expr Expr
    | ShiftR Expr Expr

    | TernIf Expr Expr Expr -- ^ If expr 1 evaluates to true, then expr 2, else expr 3
    | VecIdx Expr Expr -- ^ Index expr, name of vector variable
    | FunCall [Expr] Expr -- ^ Arg exprs, name of function variable

    -- Assignment primitive operations
    | Assign Expr Expr
    | AssignAdd Expr Expr
    | AssignSub Expr Expr
    | AssignMul Expr Expr
    | AssignDiv Expr Expr
    | AssignMod Expr Expr
    | AssignBitOr Expr Expr
    | AssignBitAnd Expr Expr
    | AssignShiftL Expr Expr
    | AssignShiftR Expr Expr
    deriving (Eq, Ord, Show)

-- Whitespace and comment consumer
sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String 
symbol = L.symbol sc

-- Parser for a variable name
pVariable :: Parser Expr
pVariable = Var <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

-- Parsers for number types
pInteger :: Parser Expr
pInteger = IntT <$> lexeme L.decimal

pOctal :: Parser Expr
pOctal = IntT <$> lexeme (char '0' >> L.octal)

pFloat :: Parser Expr
pFloat = FloatT <$> lexeme L.float

pString :: Parser Expr
pString = StringT <$> lexeme (between (symbol "\"") (symbol "\"") contents)
  where
    -- Replace \" with ", all other escapes should be fine?
    contents = many (escape <|> noneOf "\"")
    escape = char '\\' >> char '"'

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm = choice
    [ parens pExpr
    , pVariable
    , try pFloat
    , pOctal
    , pInteger
    , pString
    ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

-- Mostly following C operator precedence
operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [vecIdx, funCall, postfix "--" DecR, postfix "++" IncR]
    , [Prefix unaryPrefixOps]
    , [binaryNF '=' "*" Mul, binaryNF '=' "/" Div, binaryNF '=' "%" Mod]
    , [binaryNF '=' "+" Add, binaryNF '=' "-" Sub]
    , [binaryNF '=' "<<" ShiftL, binaryNF '=' ">>" ShiftR]
    , [binary ">=" Ge, binary "<=" Le, binaryNF '>' ">" Gt, binaryNF '<' "<" Lt]
    , [binary "==" Eq, binary "!=" NotEq]
    , [binaryNF '=' "&" BitAnd]
    , [binaryNF '=' "|" BitOr]
    , [ternary]
    , [binary "=" Assign, binary "+=" AssignAdd, binary "-=" AssignSub, binary "*=" AssignMul, binary "/=" AssignDiv
      , binary "%=" AssignMod, binary "&=" AssignBitAnd, binary "|=" AssignBitOr, binary "<<=" AssignShiftL
      , binary ">>=" AssignShiftR]
    ]

-- Necessary for parsing multiple unary operations in a row
unaryPrefixOps :: Parser (Expr -> Expr)
unaryPrefixOps = foldr1 (.) <$> some (
        DecL <$ symbol "--" <|> 
        IncL <$ symbol "++" <|> 
        Neg <$ symbol "-" <|> 
        Deref <$ symbol "*" <|>
        Addr <$ symbol "&" <|>
        Not <$ symbol "!"
    )

binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

-- Binary operation not followed by a character, used for maximal munch above
binaryNF :: Char -> String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binaryNF notEnd name f = InfixL $ do
    try $ symbol name >> notFollowedBy (char notEnd)
    return f

-- prefix :: String -> (Expr -> Expr) -> Operator Parser Expr
-- prefix name f = Prefix (f <$ symbol name)

postfix :: String -> (Expr -> Expr) -> Operator Parser Expr
postfix name f = Postfix (f <$ symbol name)

-- Only one type of ternary here: (check ? true : false)
ternary :: Operator Parser Expr
ternary = TernR ((TernIf <$ symbol ":") <$ symbol "?")

-- Vector indexing, gets the expr between brackets
vecIdx :: Operator Parser Expr
vecIdx = Postfix (VecIdx <$> between (symbol "[") (symbol "]") pExpr)

-- Function calls, gets the comma separated argument expr list between parens
funCall :: Operator Parser Expr
funCall = Postfix (FunCall <$> parens (pExpr `sepBy` symbol ","))

-- Check if an expression is a compile time constant and evaluate it
-- According to the spec, this means
-- Any valid combination of numeric or character constants, unary operators, binary operators, and parentheses
-- No assignment should happen here
-- It also "must give an integer result", so floating point numbers are not allowed
evalConstExpr :: Expr -> Maybe Int
evalConstExpr (IntT i) = Just i
evalConstExpr (Neg e) = do
    res <- evalConstExpr e
    return $ -res
evalConstExpr (Not e) = do
    res <- evalConstExpr e
    return (if res == 0 then 1 else 0)
evalConstExpr (Add a b) = binConstExpr (+) a b
evalConstExpr (Sub a b) = binConstExpr (-) a b
evalConstExpr (Mul a b) = binConstExpr (*) a b
evalConstExpr (Div a b) = binConstExpr (div) a b
evalConstExpr (Mod a b) = binConstExpr (mod) a b
evalConstExpr (Ge a b) = binConstExpr (\x y -> if x >= y then 1 else 0) a b
evalConstExpr (Gt a b) = binConstExpr (\x y -> if x > y then 1 else 0) a b
evalConstExpr (Le a b) = binConstExpr (\x y -> if x <= y then 1 else 0) a b
evalConstExpr (Lt a b) = binConstExpr (\x y -> if x < y then 1 else 0) a b
evalConstExpr (Eq a b) = binConstExpr (\x y -> if x == y then 1 else 0) a b
evalConstExpr (NotEq a b) = binConstExpr (\x y -> if x /= y then 1 else 0) a b
evalConstExpr (BitAnd a b) = binConstExpr (.&.) a b
evalConstExpr (BitOr a b) = binConstExpr (.|.) a b
evalConstExpr (ShiftL a b) = binConstExpr (shiftL) a b
evalConstExpr (ShiftR a b) = binConstExpr (shiftR) a b
evalConstExpr _ = Nothing

-- Binary constant expression evaluation
binConstExpr :: (Int -> Int -> Int) -> Expr -> Expr -> Maybe Int
binConstExpr f a b = do
    aVal <- evalConstExpr a
    bVal <- evalConstExpr b
    return $ f aVal bVal

-- Roughly corresponds to the `statement` in the B grammar
-- Some weird constructs such as chained declarations have been taken out
-- (ex: auto a; b is not treated like one statement here; this is only relevant for one line for loops and other silly things)
data Statement
    = Auto [(String, Maybe Expr)]
    | Extern [String]
    | Label String
    | Case Expr Statement
    | Compound [Statement]
    | If Expr Statement
    | IfElse Expr Statement Statement
    | While Expr Statement
    | Switch Expr Statement
    | Goto String
    | Return Expr
    | ExprT Expr