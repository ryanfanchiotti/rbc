module BC.Syntax (
    Expr(..),
    Statement(..),
    Definition(..),
    Program,
    VarName
) where

type VarName = String

-- Essentially an RValue, will need to be checked for LValues in
-- assignment, increment, decrement, address at a later stage
data Expr
    = Var VarName -- ^ Variable name

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
    | VecIdx Expr Expr -- ^ Index expr, name of vector variable (or address to dereference, etc)
    | FunCall [Expr] Expr -- ^ Arg exprs, name of function variable (or function pointer)

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

-- Roughly corresponds to `statement` in the B grammar
-- Some weird constructs such as chained declarations have been taken out
-- (ex: auto a; b is not treated like one statement here; this is only relevant for one line while loops and other silly things)
data Statement
    = Auto [(VarName, Maybe Expr)]
    | Extern [VarName]
    | LabelDec VarName -- ex: L2:
    -- Note that case isn't chained to another statement on purpose because switches not within compound statements are usually not intended
    -- This should also always have a const-expr according to the spec
    | Case Expr
    | Compound [Statement] -- {a; b; c;}
    | If Expr Statement
    | IfElse Expr Statement Statement
    | While Expr Statement
    | Switch Expr Statement
    | Goto VarName
    | Return (Maybe Expr)
    | ExprT Expr
    deriving (Eq, Ord, Show)

-- A top level definition
data Definition
    = Func VarName [VarName] Statement -- Function def: name, args, statement
    | Global VarName (Maybe Expr) -- Name, init-val
    | GlobalVec VarName (Maybe Expr) (Maybe [Expr]) -- Name, size, init-list
    deriving (Eq, Ord, Show)

type Program = [Definition]