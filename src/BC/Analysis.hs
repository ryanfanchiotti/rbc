module BC.Analysis (
    evalConstExpr,
    isLValue,
    analyzeExpr,
    analyzeStatement,
    ParentS(..)
) where

import BC.Syntax
import Data.Bits
import Data.List
import qualified Data.HashSet as S

type Error = String
type DefinedVars = S.HashSet String

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
    a_val <- evalConstExpr a
    b_val <- evalConstExpr b
    return $ f a_val b_val

-- Can we assign to this expr?
isLValue :: Expr -> Either Error Expr
isLValue e@(Var _) = Right e
isLValue e@(Deref _) = Right e
isLValue e@(VecIdx _ _) = Right e
isLValue _ = Left $ "assignment can only be done to vars, derefs, or indexed vecs"

-- Are this Expr and the nested Exprs inside of it valid?
-- Perform constant folding where possible
analyzeExpr :: DefinedVars -> Expr -> Either Error Expr
analyzeExpr dv e 
    | (Just const_int) <- evalConstExpr e = Right (IntT const_int)
    | (Var name) <- e = if S.member name dv then Right e else Left $ name ++ " used before definition"
    | (IntT _) <- e = Right e
    | (FloatT _) <- e = Right e
    | (StringT _) <- e = Right e
    | (Neg ne) <- e = analyzeExpr dv ne
    | (Addr ne) <- e = analyzeExpr dv ne
    | (Deref ne) <- e = analyzeExpr dv ne
    | (IncL ne) <- e = analyzeExpr dv ne
    | (IncR ne) <- e = analyzeExpr dv ne
    | (DecL ne) <- e = analyzeExpr dv ne
    | (DecR ne) <- e = analyzeExpr dv ne
    | (Not ne) <- e = analyzeExpr dv ne
    | (Add a b) <- e = analyzeBinExpr dv Add a b
    | (Sub a b) <- e = analyzeBinExpr dv Sub a b
    | (Mul a b) <- e = analyzeBinExpr dv Mul a b
    | (Div a b) <- e = analyzeBinExpr dv Div a b
    | (Mod a b) <- e = analyzeBinExpr dv Mod a b
    | (Gt a b) <- e = analyzeBinExpr dv Gt a b
    | (Ge a b) <- e = analyzeBinExpr dv Ge a b
    | (Lt a b) <- e = analyzeBinExpr dv Lt a b
    | (Le a b) <- e = analyzeBinExpr dv Le a b
    | (Eq a b) <- e = analyzeBinExpr dv Eq a b
    | (NotEq a b) <- e = analyzeBinExpr dv NotEq a b
    | (BitOr a b) <- e = analyzeBinExpr dv BitOr a b
    | (BitAnd a b) <- e = analyzeBinExpr dv BitAnd a b
    | (ShiftL a b) <- e = analyzeBinExpr dv ShiftL a b
    | (ShiftR a b) <- e = analyzeBinExpr dv ShiftR a b
    | (VecIdx a b) <- e = analyzeBinExpr dv VecIdx a b
    | (TernIf a b c) <- e = do
        aexpr <- analyzeExpr dv a
        bexpr <- analyzeExpr dv b
        cexpr <- analyzeExpr dv c
        return $ TernIf aexpr bexpr cexpr
    | (FunCall a b) <- e = do
        aexprseq <- mapM (analyzeExpr dv) a
        bexpr <- analyzeExpr dv b
        return $ FunCall aexprseq bexpr
    | (Assign a b) <- e = analyzeAssignExpr dv Assign a b
    | (AssignAdd a b) <- e = analyzeAssignExpr dv AssignAdd a b
    | (AssignSub a b) <- e = analyzeAssignExpr dv AssignSub a b
    | (AssignMul a b) <- e = analyzeAssignExpr dv AssignMul a b
    | (AssignDiv a b) <- e = analyzeAssignExpr dv AssignDiv a b
    | (AssignMod a b) <- e = analyzeAssignExpr dv AssignMod a b
    | (AssignBitOr a b) <- e = analyzeAssignExpr dv AssignBitOr a b
    | (AssignBitAnd a b) <- e = analyzeAssignExpr dv AssignBitAnd a b
    | (AssignShiftL a b) <- e = analyzeAssignExpr dv AssignShiftL a b
    | (AssignShiftR a b) <- e = analyzeAssignExpr dv AssignShiftL a b

analyzeBinExpr :: DefinedVars -> (Expr -> Expr -> Expr) -> Expr -> Expr -> Either Error Expr
analyzeBinExpr dv dcon a b = do
    aexpr <- analyzeExpr dv a
    bexpr <- analyzeExpr dv b
    return $ dcon aexpr bexpr

analyzeAssignExpr :: DefinedVars -> (Expr -> Expr -> Expr) -> Expr -> Expr -> Either Error Expr
analyzeAssignExpr dv dcon a b = do
    _ <- isLValue a
    analyzeBinExpr dv dcon a b

-- Parent scope, needed for case placement checking
data ParentS = SwitchS | OtherS deriving (Eq, Show, Ord)

-- Info needed to determine if we are going to a nonexistent label
type Labels = S.HashSet String
type Gotos = S.HashSet String

type StatementInfo = (DefinedVars, Statement, Labels, Gotos)

-- Analyze and perform simple optimizations on a Statement
analyzeStatement :: StatementInfo -> ParentS -> Either Error StatementInfo
analyzeStatement si ps
    | (Auto lst) <- s = do
        exprs <- mapM (autoExpr dv) lst
        let name_strs = map fst lst
        let names = S.fromList name_strs
        let new_stmt = Auto $ zip name_strs exprs
        let name_overlap = S.intersection names dv
        let list_str = intercalate ", " (sort $ S.toList name_overlap)
        if name_overlap /= S.empty
            then Left $ "auto vars " ++ list_str ++ " already defined" 
            else return (S.union dv names, new_stmt, labels, gotos)
    | (Extern vns) <- s = let 
                            name_overlap = S.intersection (S.fromList vns) dv
                            list_str = intercalate ", " (sort $ S.toList name_overlap)
                          in if name_overlap /= S.empty
                                then Left $ "extern vars " ++ list_str ++ " already defined" 
                                else Right (S.union (S.fromList vns) dv, s, labels, gotos)
    | (LabelDec vn) <- s = if S.member vn dv
                                then Left $ "label " ++ vn ++ " already defined"
                                else Right (dv, s, S.insert vn labels, gotos)
    | (Case e) <- s, Just _ <- evalConstExpr e, ps == SwitchS = do
        new_e <- analyzeExpr dv e
        return (dv, Case new_e, labels, gotos)
    | (Case _) <- s, ps /= SwitchS = Left "case expr should be under switch scope"
    | (Case _) <- s = Left "case expr is not constant"
    -- Compound and control flow statements should not add to defined vars for current scope
    | (Compound stmts) <- s = do
        (stmts', labels', gotos') <- analyzeComp si ps stmts
        return (dv, Compound stmts', labels', gotos')
    | (If e stmt) <- s = do
        e' <- analyzeExpr dv e
        (_, stmt', l, g) <- analyzeStatement (dv, stmt, labels, gotos) OtherS
        return (dv, If e' stmt', l, g)
    | (IfElse e f_stmt s_stmt) <- s = do
        e' <- analyzeExpr dv e
        (_, f_stmt', l_f, g_f) <- analyzeStatement (dv, f_stmt, labels, gotos) OtherS
        (_, s_stmt', l_s, g_s) <- analyzeStatement (dv, s_stmt, labels, gotos) OtherS
        return (dv, IfElse e' f_stmt' s_stmt', S.union l_f l_s, S.union g_f g_s)
    | (While e stmt) <- s = do
        e' <- analyzeExpr dv e
        (_, stmt', l, g) <- analyzeStatement (dv, stmt, labels, gotos) OtherS
        return (dv, While e' stmt', l, g)
    | (Switch e stmt) <- s = do
        e' <- analyzeExpr dv e
        (_, stmt', l, g) <- analyzeStatement (dv, stmt, labels, gotos) SwitchS
        return (dv, Switch e' stmt', l, g)
    | (Goto vn) <- s = Right (dv, s, labels, S.insert vn gotos)
    | (Return (Just e)) <- s = do
        e' <- analyzeExpr dv e
        return (dv, Return (Just e'), labels, gotos)
    | (Return _) <- s = Right si
    | (ExprT e) <- s = do
        e' <- analyzeExpr dv e
        return (dv, ExprT e', labels, gotos)
    where
        (dv, s, labels, gotos) = si

        autoExpr _ (_, Nothing) = Right Nothing
        autoExpr defvars (_, Just expr) = do
            new_expr <- analyzeExpr defvars expr
            return (Just new_expr)

-- Analyze a compound statement
analyzeComp :: StatementInfo -> ParentS -> [Statement] -> Either Error ([Statement], Labels, Gotos)
analyzeComp (defv, cmpd, l, g) par (x:xs) = case x of
    -- Any code past Return in a compound statement is dead
    r@(Return _) -> Right ([r], l, g)
    _ -> do
        (defv', st', l', g') <- analyzeStatement (defv, x, l, g) par
        (next, l'', g'') <- analyzeComp (defv', cmpd, l', g') par xs
        return (st' : next, l'', g'')
analyzeComp (_, _, l, g) _ [] = Right ([], l, g)

-- Ensure AST is valid before code generation
analyzeProg :: DefinedVars -> Labels -> Gotos -> Program -> (DefinedVars -> Labels -> Gotos)
analyzeProg dv l g prog = undefined
        
