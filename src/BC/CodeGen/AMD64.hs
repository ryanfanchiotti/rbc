module BC.CodeGen.AMD64 (
    emitProg,
    emitDef,
    emitStmt,
    emitExpr,
    FuncState(..)
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
-- (RBP - address), preserved across func calls

-- NO POINTER ARITHMETIC (cannot tell what vars are pointers in an untyped lang)
-- Vectors -> Start address + index * 8 (often, this means a[b] != b[a])

-- Strings are put into .data as ascii with a null terminator. For stack allocated strings,
-- use Vecs and treat each 64 bit value as having 8 characters.
-- This can be done with the syntax `abcdefgh` replacing a quadword

import BC.Syntax
import BC.Analysis
import BC.NameUtils
import Data.List
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

-- Variable offset in the stack as a string
getVarOffset :: FuncState -> VarName -> String
getVarOffset fs vn = case HM.lookup vn (var_loc fs) of
                        (Just i) -> show i
                        Nothing -> error ("offset of var " ++ vn ++ " not found in\n"
                            ++ show fs ++ "\nshould be unreachable")

-- Type associated with a variable
getVarType :: FuncState -> VarName -> VarType
getVarType fs vn = case HM.lookup vn (var_type fs) of
                        (Just t) -> t
                        Nothing -> error ("type of var " ++ vn ++ " not found in\n"
                            ++ show fs ++ "\nshould be unreachable")

-- Take a program and return lines to be mapped to a file
emitProg :: Program -> [String]
emitProg p = let (dt, ct, ns) = emitProg' p makeNameState
             in map ((".global " ++) . defName) p ++ [".data"] ++ dt ++ [".text"] ++ ct

emitProg' :: Program -> NameState -> (DataText, CodeText, NameState)
emitProg' (def:defs) ns = let (dt, ct, ns') = emitDef def ns
                              (dt2, ct2, ns'') = emitProg' defs ns'
                          in (dt ++ dt2, ct ++ ct2, ns'')
emitProg' [] ns = ([], [], ns)

emitDef :: Definition -> NameState -> (DataText, CodeText, NameState)
emitDef (Func name args stmt) ns = let lab = [name ++ ":"]
                                       (prelude, fs) = emitPrelude args ns
                                       (dt, ct, fs') = emitStmt stmt fs
                                       post = if name == "main" then ["    xor %rax,%rax"] ++ emitReturn else []
                                   in (dt, lab ++ prelude ++ ct ++ post, name_state fs')
emitDef (Global name (Just (IntT i))) ns = ([name ++ ": .quad " ++ show i], [], ns)
emitDef (Global name Nothing) ns = ([name ++ ": .quad 0"], [], ns)
emitDef (GlobalVec name m_size m_init_list) ns = undefined
emitDef d _ = error $ "non const global\n" ++ show d
                ++ "\nshould be unreachable"

emitReturn :: [String]
emitReturn = ["    mov %rbp,%rsp", "    pop %rbp", "    ret"]

-- Push RBP and rest of args onto stack and note location in function state
emitPrelude :: [VarName] -> NameState -> ([String], FuncState)
emitPrelude args ns = let reg_list = ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"]
                          regs = zip args reg_list
                          setup_frame = ["    push %rbp", "    mov %rsp,%rbp"]
                          setup_args = map ("    push " ++) (take (length args) reg_list)
                          fs = FuncState { var_loc = HM.fromList (map (\(idx, arg) -> (arg, 8 + (8 * idx))) (zip [0..] args))
                                         , var_type = HM.fromList (zip args (repeat AutoT))
                                         , sp_loc = (length args) * 8
                                         , name_state = ns }
                      in (setup_frame ++ setup_args, fs)

emitStmt :: Statement -> FuncState -> (DataText, CodeText, FuncState)
emitStmt s fs
    | (Auto vns) <- s = emitAutos vns fs
    -- Externs are dealt with automatically by gas
    | (Extern vns) <- s = ([], [], addExterns vns fs)
    | (LabelDec ln) <- s = ([], [ln ++ ":"], fs)
    | (Case e) <- s = undefined
    | (Compound stmts) <- s = emitCompounds stmts fs
    | (If e stmt) <- s = let ns = name_state fs
                             (end_lab, ns') = makeLabel ns
                             fs' = fs {name_state = ns'}

                             (dt, ct, fs'') = emitExpr e fs'
                             goto_end = ["    cmp $0, %rax", "    je " ++ end_lab]

                             (dt2, ct2, fs''') = emitStmt stmt fs''

                             diff = (sp_loc fs''') - (sp_loc fs)
                             restore_stack = ["    add $" ++ show diff ++ ",%rsp"]

                             unscoped_fs = fs {name_state = name_state fs'''}
                         in (dt ++ dt2,
                            ct ++ goto_end ++ ct2 ++ restore_stack ++ [end_lab ++ ":"],
                            unscoped_fs)
    | (IfElse e stmt_f stmt_s) <- s = let ns = name_state fs
                                          (else_lab, ns') = makeLabel ns
                                          (end_lab, ns'') = makeLabel ns'
                                          fs' = fs {name_state = ns''}

                                          (dt, ct, fs'') = emitExpr e fs'
                                          goto_else = ["    cmp $0, %rax", "    je " ++ else_lab]

                                          (dt2, ct2, fs''') = emitStmt stmt_f fs''
                                          (dt3, ct3, fs'''') = emitStmt stmt_s fs'' {name_state = name_state fs'''}

                                          diff2 = (sp_loc fs''') - (sp_loc fs)
                                          diff3 = (sp_loc fs'''') - (sp_loc fs)

                                          restore_stack2 = ["    add $" ++ show diff2 ++ ",%rsp"]
                                          restore_stack3 = ["    add $" ++ show diff3 ++ ",%rsp"]

                                          jmp_end = ["    jmp " ++ show end_lab]

                                          unscoped_fs = fs {name_state = name_state fs''''}
                                      in (dt ++ dt2 ++ dt3,
                                         ct ++ goto_else ++ ct2 ++ restore_stack2 ++ jmp_end ++ [else_lab ++ ":"]
                                         ++ ct3 ++ restore_stack3 ++ [end_lab ++ ":"],
                                         unscoped_fs)
    | (While e stmt) <- s = let ns = name_state fs
                                (start_lab, ns') = makeLabel ns
                                (end_lab, ns'') = makeLabel ns'
                                fs' = fs {name_state = ns''}

                                (dt, ct, fs'') = emitExpr e fs'
                                goto_end = ["    cmp $0, %rax", "    je " ++ end_lab]

                                (dt2, ct2, fs''') = emitStmt stmt fs''

                                diff = (sp_loc fs''') - (sp_loc fs)
                                restore_stack = ["    add $" ++ show diff ++ ",%rsp"]
                                goto_start = ["    jmp " ++ start_lab]

                                unscoped_fs = fs {name_state = name_state fs'''}
                            in (dt ++ dt2,
                               [start_lab ++ ":"] ++ ct ++ goto_end ++ ct2 ++ restore_stack ++ goto_start ++ [end_lab ++ ":"],
                               unscoped_fs)
    | (Switch e stmt) <- s = undefined
    | (Goto ln) <- s = ([], ["    jmp " ++ ln], fs)
    | (Return (Just e)) <- s = let (dt, ct, fs') = emitExpr e fs
                               in (dt, ct ++ emitReturn, fs')
    | (Return Nothing) <- s = ([], ["    xor %rax,%rax"] ++ emitReturn, fs)
    | (ExprT e) <- s = emitExpr e fs

-- Autos have optional const-expr size
emitAutos :: [(VarName, Maybe Expr)] -> FuncState -> (DataText, CodeText, FuncState)
emitAutos ((vn, Nothing):vns) fs = let fs' = fs { sp_loc = sp_loc fs + 8
                                                , var_loc = HM.insert vn (sp_loc fs + 8) (var_loc fs)
                                                , var_type = HM.insert vn AutoT (var_type fs) }
                                       (dt, ct, fs'') = emitAutos vns fs'
                                   in (dt, ["    pushq $0"] ++ ct, fs'')
emitAutos ((vn, (Just (IntT vec_size))):vns) fs = undefined
emitAutos ((_, _):_) _ = error "non-const auto size, should be unreachable"
emitAutos [] fs = ([], [], fs)

addExterns :: [VarName] -> FuncState -> FuncState
addExterns vns fs = fs { var_type = foldl' (\hm x -> HM.insert x ExternT hm) (var_type fs) vns }

-- Use original function state, except for name state; bring back stack pointer
emitCompounds :: [Statement] -> FuncState -> (DataText, CodeText, FuncState)
emitCompounds sts fs = let (dt, ct, fs') = emitCompounds' sts fs
                       in (dt,
                           ct ++ ["    add $" ++ show ((sp_loc fs') - (sp_loc fs)) ++ ",%rsp"],
                           fs {name_state = name_state fs'})

emitCompounds' :: [Statement] -> FuncState -> (DataText, CodeText, FuncState)
emitCompounds' (st:sts) fs = let (dt, ct, fs') = emitStmt st fs
                                 (dt2, ct2, fs'') = emitCompounds' sts fs'
                             in (dt ++ dt2, ct ++ ct2, fs'')
emitCompounds' [] fs = ([], [], fs)

-- Put the address to be assigned to into RAX
-- This returns the LValue context (where to assign)!
emitAddrExpr :: Expr -> FuncState -> (DataText, CodeText, FuncState)
emitAddrExpr e fs
    | (Var vn) <- e, getVarType fs vn == AutoT =
        ([], ["    mov %rbp,%rax", "    sub $" ++ (getVarOffset fs vn) ++ ",%rax"], fs)
    | (Var vn) <- e, getVarType fs vn == ExternT = ([], ["    lea " ++ vn ++ "(%rip),%rax"], fs)
    -- Address will simply be the result of expr
    | (Deref expr) <- e = emitExpr expr fs
    -- Expr + index * 8
    | (VecIdx index name) <- e = emitExpr (Add (Mul index (IntT 8)) name) fs
    | otherwise = error $ "emitAddrExpr called on\n" ++ show e ++ 
        "\nwhich is not an LValue, should be unreachable"

-- Put the result of an expression into RAX
-- This returns the RValue context (what is inside a variable, etc)!
-- Emitting an expression does not change the stack state,
-- all pushes are cleaned up
emitExpr :: Expr -> FuncState -> (DataText, CodeText, FuncState)
emitExpr e fs
    -- Auto vars are on the stack, extern vars are in .data
    | (Var vn) <- e, getVarType fs vn == AutoT =
        ([], ["    mov -" ++ (getVarOffset fs vn) ++ "(%rbp),%rax"], fs)
    | (Var vn) <- e, getVarType fs vn == ExternT =
        ([], ["    mov " ++ vn ++ "(%rip),%rax"], fs)
    | (IntT i) <- e = ([], ["    mov $" ++ (show i) ++ ",%rax"], fs)
    -- Floating point numbers are stored as integers here
    | (FloatT d) <- e = error $ "emitExpr todo: " ++ (show e)
    -- Strings are in pointers to null terminated strings in .data
    | (StringT s) <- e = let
                            (name, ns) = makeName $ name_state fs
                            fs' = fs { name_state = ns }
                         in
                         ([name ++ ": .asciz \"" ++ s ++ "\""], ["    lea " ++ name ++ "(%rip),%rax"], fs')
    | (Neg expr) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Deref expr) <- e = let (dt, ct, fs') = emitExpr expr fs
                              deref = ["    mov (%rax),%rax"]
                          in (dt, ct ++ deref, fs')
    | (Not expr) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Add expr expr_s) <- e = emitBinOp expr expr_s ["    add %r10,%rax"] fs
    | (Sub expr expr_s) <- e = emitBinOp expr expr_s ["    sub %r10,%rax"] fs
    | (Mul expr expr_s) <- e = emitBinOp expr expr_s ["    imul %r10,%rax"] fs
    | (Div expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Mod expr expr_s) <- e = emitBinOp expr expr_s ["    xor %rdx,%rdx", "    idiv %r10", "    mov %rdx,%rax"] fs
    | (Gt expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Ge expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Lt expr expr_s) <- e = emitBinOp expr expr_s ["    cmp %r10,%rax", "    setl %al", "    movzx %al,%rax"] fs
    | (Le expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (Eq expr expr_s) <- e = emitBinOp expr expr_s ["    cmp %r10,%rax", "    setz %al", "    movzx %al,%rax"] fs
    | (NotEq expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (BitOr expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (BitAnd expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (ShiftL expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (ShiftR expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (TernIf cond true_e false_e) <- e = error $ "emitExpr todo: " ++ (show e)
    | (VecIdx idx name) <- e = emitExpr (Deref (Add name (Mul idx (IntT 8)))) fs
    -- When a function is called on an LValue, use the LValue context when
    -- trying to call what is at the given address
    | (FunCall args addr) <- e, isLValueB addr = funCall addr args emitAddrExpr
    | (FunCall args addr) <- e = funCall addr args emitExpr

    -- For assignment statements, finding the LValue context is needed
    -- Otherwise we will assign to variable contents, etc and probably
    -- get segfaults

    | (Addr expr) <- e = emitAddrExpr expr fs
    | (IncL expr) <- e = emitExpr (Assign expr (Add expr (IntT 1))) fs
    | (IncR expr) <- e = error $ "emitExpr todo: " ++ (show e)
    | (DecL expr) <- e = error $ "emitExpr todo: " ++ (show e)
    | (DecR expr) <- e = error $ "emitExpr todo: " ++ (show e)
    -- Leave the returned value in RAX, so as to return it
    | (Assign expr expr_s) <- e = let
                                    (dt, ct, fs') = emitAddrExpr expr fs
                                    tmp = ["    push %rax"]
                                    (dt2, ct2, fs'') = emitExpr expr_s (fs' {sp_loc = sp_loc fs' + 8})
                                    move = ["    pop %r10", "    mov %rax,(%r10)"]
                                  in (dt ++ dt2, ct ++ tmp ++ ct2 ++ move, fs'' {sp_loc = sp_loc fs'' - 8})
    | (AssignAdd expr expr_s) <- e = emitExpr (Assign expr (Add expr expr_s)) fs
    | (AssignSub expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (AssignMul expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (AssignDiv expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (AssignMod expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (AssignBitOr expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (AssignBitAnd expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (AssignShiftL expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)
    | (AssignShiftR expr expr_s) <- e = error $ "emitExpr todo: " ++ (show e)

    where
        funCall addr args emitType = let
                                        (dt, ct, fs') = emitType addr fs
                                        (dt2, ct2, fs'') = emitFunCall args fs'
                                     in (dt ++ dt2, ct ++ ct2, fs'')

-- For operation lines, assumes two args are in RAX and R10, result is in RAX
emitBinOp :: Expr -> Expr -> [String] -> FuncState -> (DataText, CodeText, FuncState)
emitBinOp expr expr_s op_lines fs = let
                                    (dt1, ct1, fs') = emitExpr expr_s fs
                                    (dt2, ct2, fs'') = emitExpr expr (fs' {sp_loc = sp_loc fs' + 8})
                                    ct3 = ct1 ++ ["    push %rax"] ++ ct2 ++ ["    pop %r10"] ++ op_lines
                                 in
                                 (dt1 ++ dt2, ct3, fs'' {sp_loc = sp_loc fs'' - 8})

-- Stack pointer must be 16 byte aligned, it will always be 8 byte aligned here
-- RAX must have 0 to state that no sse registers are used
-- Assumes address to call is in RAX, and moves it to R11
emitFunCall :: [Expr] -> FuncState -> (DataText, CodeText, FuncState)
emitFunCall es fs = let
                        regs = (map Just ["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"]) ++ (repeat Nothing)
                        new_es = zip es regs

                        -- 8 extra bytes come from pushing RBP in the prelude
                        total_stack = sp_loc fs + 8
                        needs_align = total_stack `mod` 16 /= 0

                        push_loc = ["    push %rax"]
                        -- Use `total_stack` here to denote the extra push, not the stored RBP as before
                        (move_d, move_c, fs') = moveArgs new_es fs { sp_loc = total_stack }
                        -- Get function to call that was pushed from RAX
                        call_pre = ["    xor %rax,%rax",
                                    "    pop %r11"]
                        align_inst = if needs_align
                                        then ["    sub $8,%rsp"]
                                        else []
                        call = ["    call *%r11"]
                        -- Move the stack pointer back where it should be
                        adj_stack = if needs_align then ["    add $8,%rsp"] else []
                    -- Use original function state, since the location was popped off the stack
                    in (move_d, push_loc ++ move_c ++ call_pre ++ align_inst ++ call ++ adj_stack, fs' {sp_loc = sp_loc fs' - 8})

moveArgs :: [(Expr, Maybe String)] -> FuncState -> (DataText, CodeText, FuncState)
moveArgs [] fs = ([], [], fs)
moveArgs ((e, Just reg):es) fs = let
                                    (dt, ct, fs') = emitExpr e fs
                                    (dt2, ct2, fs'') = moveArgs es fs'
                                 in (dt ++ dt2, ct ++ ["    mov %rax," ++ reg] ++ ct2, fs'')
moveArgs ((_, Nothing):_) _ = error "TODO: function calls with more than 6 args"

