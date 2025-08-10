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
    , name_state :: NameState
    } deriving (Eq, Show, Ord)

emitProg :: Program -> (DataText, CodeText)
emitProg p = undefined

emitStmt :: Statement -> (DataText, CodeText)
emitStmt s = undefined

emitExpr :: Expr -> FuncState -> (DataText, CodeText, FuncState)
emitExpr e fs = undefined