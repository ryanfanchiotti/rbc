module BC.Analysis (
    evalConstExpr,
    isLValue
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
isLValue _ = Left "assignment can only be done to vars, derefs, or indexed vecs"

-- Are this Expr and the nested Exprs inside of it valid?
checkExpr :: Expr -> Either Error Expr
checkExpr = undefined