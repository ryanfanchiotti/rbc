module BC.CodeGen.AMD64 (
    emitProg
) where

-- Code generation for AMD64 Linux
-- Calling convention: RDI, RSI, RDX, RCX, R8, R9 are function args,
--                     rest is pushed onto stack from right to left
-- RAX: temp register, 1st return register, not preserved across func calls
--      on function calls, amount of sse registers used. Set to zero before calls!
-- RBX, R12 - R15: preserved across func calls
-- R10, R11: not preserved across func calls
-- RSP: stack pointer, must be 16 byte aligned across function calls
-- RBP: frame pointer, used for accessing variables:
-- (RBP - 8 for saved old RBP - address), preserved across func calls

-- NO POINTER ARITHMETIC (cannot tell what vars are pointers in an untyped lang)
-- Vectors -> Start address + index * 8 (often, this means a[b] != b[a])

-- Strings are put into .data as ascii with a null terminator. For stack allocated strings,
-- use Vecs and treat each 64 bit value as having 8 characters.
-- This can be done with the syntax `abcdefgh` replacing a quadword

import BC.Syntax
import BC.Analysis
import BC.NameUtils
import qualified Data.HashMap.Lazy as HM

type DataText = [String]
type CodeText = [String]
data VarType = AutoT | ExternT deriving (Eq, Show, Ord)

data FuncState = FuncState
    -- Where a variable is on the stack (RBP - x)
    { var_loc :: HM.HashMap VarName Int
    -- The type of each variable
    , var_type :: HM.HashMap VarName VarType
    -- Stack pointer location (or difference between stack ptr and frame ptr)
    , sp_loc :: Int
    -- State for name generation
    , name_state :: NameState
    } deriving (Eq, Show, Ord)

-- Take a program and return lines to be mapped to a file
emitProg :: Program -> [String]
emitProg p = undefined

emitDef :: Definition -> (DataText, CodeText)
emitDef d = undefined

emitStmt :: Statement -> FuncState -> (DataText, CodeText, FuncState)
emitStmt s fs
    | (Auto vns) <- s = undefined
    | (Extern vns) <- s = undefined
    | (LabelDec ln) <- s = undefined
    | (Case e) <- s = undefined
    | (Compound stmts) <- s = undefined
    | (If e stmt) <- s = undefined
    | (IfElse e stmt_f stmt_s) <- s = undefined
    | (While e stmt) <- s = undefined
    | (Switch e stmt) <- s = undefined
    | (Goto ln) <- s = undefined
    | (Return (Just e)) <- s = undefined
    | (Return Nothing) <- s = undefined
    | (ExprT e) <- s = undefined

-- Put the address to be assigned to into RAX
-- This returns the LValue context (where to assign)!
emitAddrExpr :: Expr -> FuncState -> (DataText, CodeText, FuncState)
emitAddrExpr e fs
    | (Var vn) <- e = undefined
    | (Deref expr) <- e = undefined
    -- Expr + index * 8
    | (VecIdx index expr) <- e = undefined

-- Put the result of an expression into RAX
-- This returns the RValue context (what is inside a variable, etc)!
emitExpr :: Expr -> FuncState -> (DataText, CodeText, FuncState)
emitExpr e fs
    -- Auto vars are on the stack, extern vars are in .data
    | (Var vn) <- e, (var_type fs) HM.! vn == AutoT = 
        ([], ["    mov -" ++ (show ((var_loc fs) HM.! vn)) ++ "(%rbp),%rax"], fs)
    | (Var vn) <- e, (var_type fs) HM.! vn == ExternT = error $ "emitExpr todo: " ++ (show e)
    | (IntT i) <- e = ([], ["    mov $" ++ (show i) ++ ",%rax"], fs)
    -- Floating point numbers are stored as integers here
    | (FloatT d) <- e = error $ "emitExpr todo: " ++ (show e)
    | (StringT s) <- e = let
                            (name, ns) = makeName $ name_state fs
                            fs' = fs { name_state = ns }
                         in
                         ([name ++ ": .asciz \"" ++ s ++ "\""], ["mov $" ++ name ++ ",%rax"], fs')
    | (Neg expr) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Deref expr) <- e = error $ "emitExpr todo: " ++ (show e)
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
    | (Eq expr expr_s) <- e = let
                                    (dt1, ct1, fs') = emitExpr expr fs
                                    (dt2, ct2, fs'') = emitExpr expr_s fs'
                                    ct3 = ct1 ++ 
                                          ["    mov %rax,%r10"] ++ 
                                          ct2 ++ 
                                          ["    cmp %rax,%r10", 
                                          "    setz %al",
                                          "    movzbl %al,%eax"]
                              in
                              (dt1 ++ dt2, ct3, fs'')
    | (NotEq expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (BitOr expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (BitAnd expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (ShiftL expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (ShiftR expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (TernIf cond true_e false_e) <- e = error $ "emitExpr todo: " ++ (show e)
    | (VecIdx idx name) <- e = error $ "emitExpr todo: " ++ (show e)
    -- When a function is called on an LValue, use the LValue context when
    -- trying to call what is at the given address
    | (FunCall args addr) <- e, isLValueB e = error $ "emitExpr todo: " ++ (show e)
    | (FunCall args addr) <- e = error $ "emitExpr todo: " ++ (show e)

    -- For assignment statements, finding the LValue context is needed
    -- Otherwise we will assign to variable contents, etc and probably
    -- get segfaults

    | (Addr expr) <- e = error $ "emitExpr todo: " ++ (show e)
    | (IncL expr) <- e = error $ "emitExpr todo: " ++ (show e)
    | (IncR expr) <- e = error $ "emitExpr todo: " ++ (show e)
    | (DecL expr) <- e = error $ "emitExpr todo: " ++ (show e)
    | (DecR expr) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Assign expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
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

-- Stack pointer must be 16 byte aligned, it will always be 8 byte aligned here
-- RAX must have 0 to state that no sse registers are used
-- Assumes address to call is in RAX, and moves it to R11
-- WORK IN PROGRESS
emitFunCall :: [Expr] -> VarName -> FuncState -> (DataText, CodeText, FuncState)
emitFunCall es name fs = undefined
                        --  let
                        --     reg_map = HM.fromList [ (0, "%rdi"), 
                        --                             (1, "%rsi"), 
                        --                             (2, "%rdx"),
                        --                             (3, "%rcx"), 
                        --                             (4, "%r8"), 
                        --                             (5, "%r9") ]
                        --     regs = map ((flip HM.lookup) reg_map) [0..]
                        --     new_es = zip es regs 

                        --     extra_args = max 0 ((length es) - 6)
                        --     total_stack = sp_loc fs + extra_args * 8
                        --     align_inst = if total_stack `mod` 16 == 0
                        --                     then []
                        --                     else ["    sub $8,%rsp"]
                        --   in ([], [], fs)
    