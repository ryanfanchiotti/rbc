module BC.CodeGen.AMD64 (
    emitProg
) where

-- Code generation for AMD64 Linux
-- Calling convention: RDI, RSI, RDX, RCX, R8, R9 are function args,
--                     rest is pushed onto stack from right to left
-- RAX: temp register, 1st return register, not preserved across func calls
-- RBX, R12 - R15: preserved across func calls
-- R10, R11: not preserved across func calls
-- RSP: stack pointer, must be 16 byte aligned across function calls
-- RBP: frame pointer, used for accessing variables:
-- (RBP + 8 for saved old RBP + address), preserved across func calls

-- NO POINTER ARITHMETIC (cannot tell what vars are pointers in an untyped lang)
-- Vectors -> Start address + index * 8 (often, this means a[b] != b[a])

-- Strings are built directly on the stack using `mov byte`, and their starting address is saved

import BC.Syntax
import BC.NameUtils
import qualified Data.HashMap.Lazy as HM

type DataText = [String]
type CodeText = [String]

data FuncState = FuncState
    -- Where a variable is on the stack (RBP - ?)
    { var_loc :: HM.HashMap VarName Int
    -- Stack pointer location
    , sp_loc :: Int
    -- State for name generation
    , name_state :: NameState
    } deriving (Eq, Show, Ord)

emitProg :: Program -> (DataText, CodeText)
emitProg p = undefined

emitStmt :: Statement -> (DataText, CodeText)
emitStmt s = undefined

-- Put the result of an expression into RAX
emitExpr :: Expr -> FuncState -> (CodeText, FuncState)
emitExpr e fs
    | (Var vn) <- e = error $ "emitExpr todo: " ++ (show e)
    | (IntT i) <- e = ["    $" ++ (show i) ++ ",%rax"]
    | (FloatT d) <- e = error $ "emitExpr todo: " ++ (show e)
    | (StringT s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Neg expr) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Addr expr) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Deref expr) <- e = error $ "emitExpr todo: " ++ (show e)
    | (IncL expr) <- e = error $ "emitExpr todo: " ++ (show e)
    | (IncR expr) <- e = error $ "emitExpr todo: " ++ (show e)
    | (DecL expr) <- e = error $ "emitExpr todo: " ++ (show e)
    | (DecR expr) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Not expr) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Add expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Sub expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Mul expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Div expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Mod expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Gt expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Ge expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Lt expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Le expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Eq expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (NotEq expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (BitOr expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (BitAnd expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (ShiftL expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (ShiftR expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (TernIf cond true_e false_e) <- e = error $ "emitExpr todo: " ++ (show e)
    | (VecIdx idx name) <- e = error $ "emitExpr todo: " ++ (show e)
    | (FunCall args name) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Assign expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (AssignAdd expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (AssignSub expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (AssignMul expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (AssignDiv expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (AssignMod expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (AssignBitOr expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (AssignBitAnd expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (AssignShiftL expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (AssignShiftR expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)