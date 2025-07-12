module BC.Analysis (
    evalConstExpr,
    isLValue,
    analyzeExpr
) where

import BC.Syntax
import Data.Bits

type Error = String

-- Check if an expression is a compile time constant and evaluate it (constant folding)
-- According to the spec, this means
-- Any valid combination of numeric or character constants, unary operators, binary operators, and parentheses
-- No assignment should happen here
-- It also "must give an integer result", so floating point numbers and strings are not allowed
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

-- Can we assign to this expr?
isLValue :: Expr -> Either Error Expr
isLValue e@(Var _) = Right e
isLValue e@(Deref _) = Right e
isLValue e@(VecIdx _ _) = Right e
isLValue _ = Left $ "assignment can only be done to vars, derefs, or indexed vecs"

-- Are this Expr and the nested Exprs inside of it valid?
-- Perform constant folding where possible
analyzeExpr :: Expr -> Either Error Expr
analyzeExpr e | (Just const_int) <- evalConstExpr e = Right (IntT const_int)
analyzeExpr e@(Var _) = Right e
analyzeExpr e@(IntT _) = Right e
analyzeExpr e@(FloatT _) = Right e
analyzeExpr e@(StringT _) = Right e
analyzeExpr (Neg e) = analyzeExpr e
analyzeExpr (Addr e) = analyzeExpr e
analyzeExpr (Deref e) = analyzeExpr e
analyzeExpr (IncL e) = analyzeExpr e
analyzeExpr (IncR e) = analyzeExpr e
analyzeExpr (DecL e) = analyzeExpr e
analyzeExpr (DecR e) = analyzeExpr e
analyzeExpr (Not e) = analyzeExpr e
analyzeExpr (Add a b) = analyzeBinExpr Add a b
analyzeExpr (Sub a b) = analyzeBinExpr Sub a b
analyzeExpr (Mul a b) = analyzeBinExpr Mul a b
analyzeExpr (Div a b) = analyzeBinExpr Div a b
analyzeExpr (Mod a b) = analyzeBinExpr Mod a b
analyzeExpr (Gt a b) = analyzeBinExpr Gt a b
analyzeExpr (Ge a b) = analyzeBinExpr Ge a b
analyzeExpr (Lt a b) = analyzeBinExpr Lt a b
analyzeExpr (Le a b) = analyzeBinExpr Le a b
analyzeExpr (Eq a b) = analyzeBinExpr Eq a b
analyzeExpr (NotEq a b) = analyzeBinExpr NotEq a b
analyzeExpr (BitOr a b) = analyzeBinExpr BitOr a b
analyzeExpr (BitAnd a b) = analyzeBinExpr BitAnd a b
analyzeExpr (ShiftL a b) = analyzeBinExpr ShiftL a b
analyzeExpr (ShiftR a b) = analyzeBinExpr ShiftR a b
analyzeExpr (VecIdx a b) = analyzeBinExpr VecIdx a b
analyzeExpr (TernIf a b c) = do
        aexpr <- analyzeExpr a
        bexpr <- analyzeExpr b
        cexpr <- analyzeExpr c
        return $ TernIf aexpr bexpr cexpr
analyzeExpr (FunCall a b) = do
        aexprseq <- mapM analyzeExpr a
        bexpr <- analyzeExpr b
        return $ FunCall aexprseq bexpr
analyzeExpr (Assign a b) = analyzeAssignExpr Assign a b
analyzeExpr (AssignAdd a b) = analyzeAssignExpr AssignAdd a b
analyzeExpr (AssignSub a b) = analyzeAssignExpr AssignSub a b
analyzeExpr (AssignMul a b) = analyzeAssignExpr AssignMul a b
analyzeExpr (AssignDiv a b) = analyzeAssignExpr AssignDiv a b
analyzeExpr (AssignMod a b) = analyzeAssignExpr AssignMod a b
analyzeExpr (AssignBitOr a b) = analyzeAssignExpr AssignBitOr a b
analyzeExpr (AssignBitAnd a b) = analyzeAssignExpr AssignBitAnd a b
analyzeExpr (AssignShiftL a b) = analyzeAssignExpr AssignShiftL a b
analyzeExpr (AssignShiftR a b) = analyzeAssignExpr AssignShiftR a b

analyzeBinExpr :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Either Error Expr
analyzeBinExpr dcon a b = do
    aexpr <- analyzeExpr a
    bexpr <- analyzeExpr b
    return $ dcon aexpr bexpr

analyzeAssignExpr :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Either Error Expr
analyzeAssignExpr dcon a b = do
    _ <- isLValue a
    analyzeBinExpr dcon a b