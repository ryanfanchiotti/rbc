module BC.Parser (
    pExpr,
    pStatement,
    pDef
) where

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import qualified Text.Megaparsec.Char.Lexer as L
import BC.Syntax

type Parser = Parsec Void String

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
pVariable = Var <$> pName

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

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

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
vecIdx = Postfix (VecIdx <$> brackets pExpr)

-- Function calls, gets the comma separated argument expr list between parens
funCall :: Operator Parser Expr
funCall = Postfix (FunCall <$> parens (pExpr `sepBy` symbol ","))

pStatement :: Parser Statement
pStatement = choice
             [ pAuto
             , pExtern
             , pCase
             , pCompound
             , (try pIfElse)
             , pIf
             , pWhile
             , pSwitch
             , pGoto
             , pReturn
             , (try pExprT)
             , pLabelDec -- parses last since it overlaps with expression
             ]

pAuto :: Parser Statement
pAuto = do
    _ <- symbol "auto"
    decls <- pAutoDecl `sepBy1` (symbol ",")
    _ <- symbol ";"
    return $ Auto decls

pAutoDecl :: Parser (String, Maybe Expr)
pAutoDecl = do
    name <- pName
    size <- optional $ brackets pExpr
    return (name, size)

pExtern :: Parser Statement
pExtern = do
    _ <- symbol "extern"
    names <- pName `sepBy1` (symbol ",")
    _ <- symbol ";"
    return $ Extern names

pLabelDec :: Parser Statement
pLabelDec = do
    label_name <- pName
    _ <- symbol ":"
    return $ LabelDec label_name

pCase :: Parser Statement
pCase = do
    _ <- symbol "case"
    expr <- pExpr
    _ <- symbol ":"
    return $ Case expr

pCompound :: Parser Statement
pCompound = do
    _ <- symbol "{"
    statements <- many pStatement
    _ <- symbol "}"
    return $ Compound statements

pIf :: Parser Statement
pIf = do
    _ <- symbol "if"
    expr <- parens pExpr
    statement <- pStatement
    return $ If expr statement

pIfElse :: Parser Statement
pIfElse = do
    _ <- symbol "if"
    expr <- parens pExpr
    true_statement <- pStatement
    _ <- symbol "else"
    false_statement <- pStatement
    return $ IfElse expr true_statement false_statement

pWhile :: Parser Statement
pWhile = do
    _ <- symbol "while"
    expr <- parens pExpr
    statement <- pStatement
    return $ While expr statement

-- Note: parens around the expression are added here for regularity
pSwitch :: Parser Statement
pSwitch = do
    _ <- symbol "switch"
    expr <- parens pExpr
    statement <- pStatement
    return $ Switch expr statement

pGoto :: Parser Statement
pGoto = do
    _ <- symbol "goto"
    label_name <- pName
    _ <- symbol ";"
    return $ Goto label_name

pReturn :: Parser Statement
pReturn = do
    _ <- symbol "return"
    expr <- optional pExpr
    _ <- symbol ";"
    return $ Return expr

pExprT :: Parser Statement
pExprT = ExprT <$> (pExpr <* (symbol ";"))

pName :: Parser String
pName = lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "name (alpha numeric, starting with letter)")

pDef :: Parser Definition
pDef = choice
       [ try pFunc
       , try pGlobalVec
       , pGlobal
       ]

pFunc :: Parser Definition
pFunc = do
    name <- pName
    args <- parens $ pName `sepBy` (symbol ",")
    statement <- pStatement
    return $ Func name args statement

pGlobalVec :: Parser Definition
pGlobalVec = do
    name <- pName
    _ <- symbol "["
    size <- optional pExpr
    _ <- symbol "]"
    init_vals <- optional pInitVals
    _ <- symbol ";"
    return $ GlobalVec name size init_vals

pInitVals :: Parser [Expr]
pInitVals = do
    _ <- symbol "{"
    exprs <- pExpr `sepBy` (symbol ",")
    _ <- symbol "}"
    return exprs

pGlobal :: Parser Definition
pGlobal = do
    name <- pName
    expr <- optional pExpr
    _ <- symbol ";"
    return $ Global name expr
