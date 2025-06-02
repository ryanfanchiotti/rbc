module Parser (
    Expr(..),
    pExpr,
    evalConstExpr
) where

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- Essentially an RValue, will need to be checked for LValues in
-- assignment, increment, decrement, address
data Expr
    = Var String -- ^ Variable name

    -- Basic primitive types
    | IntT Int
    | FloatT Double
    
    -- Unary primitive operations
    | Neg Expr
    | Addr Expr
    | Deref Expr
    | IncL Expr
    | IncR Expr
    | DecL Expr
    | DecR Expr

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

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm = choice
    [ parens pExpr
    , pVariable
    , try pFloat
    , pOctal
    , pInteger
    ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

-- Mostly following C operator precedence
operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [vecIdx, funCall, postfix "--" DecR, postfix "++" IncR]
    , [prefix "--" DecL, prefix "++" IncL, prefix "-" Neg, prefix "+" id, prefix "*" Deref, prefix "&" Addr]
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

binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

-- Binary operation not followed by a character, used for maximal munch above
binaryNF :: Char -> String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binaryNF notEnd name f = InfixL $ do
    try $ symbol name >> notFollowedBy (char notEnd)
    return f

prefix :: String -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)

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
evalConstExpr (Add a b) = binConstExpr (+) a b
evalConstExpr (Sub a b) = binConstExpr (-) a b
evalConstExpr (Mul a b) = binConstExpr (*) a b
evalConstExpr (Div a b) = binConstExpr (div) a b
evalConstExpr (Ge a b) = binConstExpr (\x y -> if x >= y then 1 else 0) a b
evalConstExpr (Gt a b) = binConstExpr (\x y -> if x > y then 1 else 0) a b
evalConstExpr (Le a b) = binConstExpr (\x y -> if x <= y then 1 else 0) a b
evalConstExpr (Lt a b) = binConstExpr (\x y -> if x < y then 1 else 0) a b
evalConstExpr (Eq a b) = binConstExpr (\x y -> if x == y then 1 else 0) a b
evalConstExpr (NotEq a b) = binConstExpr (\x y -> if x /= y then 1 else 0) a b
evalConstExpr _ = Nothing

-- Binary constant expression evaluation
binConstExpr :: (Int -> Int -> Int) -> Expr -> Expr -> Maybe Int
binConstExpr f a b = do
    aVal <- evalConstExpr a
    bVal <- evalConstExpr b
    return $ f aVal bVal

-- Checking if an expression is an LValue (left side value, can be assigned to)
-- Note that putting an * before an RValue makes it an LValue
isLValue :: Expr -> Bool
isLValue = undefined