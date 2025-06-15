module ParserTest (parseSpec) where

import Control.Applicative hiding (some)
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char
import BC.Parser

-- Parsing tests for Expr (which roughly corresponds to an RValue)
checkExprs :: Spec
checkExprs = describe "tests for expressions" $ do
    it "parses simple int" $
        parse (pExpr <* eof) "" "1" `shouldParse` (IntT 1)
    it "parses addition" $
        parse (pExpr <* eof) "" "a + 1" `shouldParse` (Add (Var "a") (IntT 1))
    it "parses function calls" $
        parse (pExpr <* eof) "" "addAll(1, 2, 3)" `shouldParse` (FunCall [IntT 1, IntT 2, IntT 3] (Var "addAll"))
    it "parses add and div in the correct order" $ 
        parse (pExpr <* eof) "" "1 + 2 / 3" `shouldParse` (Add (IntT 1) (Div (IntT 2) (IntT 3)))
    it "parses ternary expressions" $
        parse (pExpr <* eof) "" "a > b ? 42 : 69" `shouldParse` (TernIf (Gt (Var "a") (Var "b")) (IntT 42) (IntT 69))
    it "parses add and mod with parens in the correct order" $ 
        parse (pExpr <* eof) "" "(1 + 2) % 3" `shouldParse` (Mod (Add (IntT 1) (IntT 2)) (IntT 3))
    it "parses both right shifts in the correct order" $ 
        parse (pExpr <* eof) "" "a >>= 3 >> b" `shouldParse` (AssignShiftR (Var "a") (ShiftR (IntT 3) (Var "b")))
    it "parses both left shifts in the correct order" $ 
        parse (pExpr <* eof) "" "a <<= 3 << b" `shouldParse` (AssignShiftL (Var "a") (ShiftL (IntT 3) (Var "b")))
    it "doesn't parse two variables in a row" $ 
        parse (pExpr <* eof) "" `shouldFailOn` "a a"
    it "doesn't parse two operators in a row" $ 
        parse (pExpr <* eof) "" `shouldFailOn` "a += / b"
    it "parses vector indices" $
        parse (pExpr <* eof) "" "a[1+2]" `shouldParse` (VecIdx (Add (IntT 1) (IntT 2)) (Var "a"))
    it "parses prefix decrement and deref" $ 
        parse (pExpr <* eof) "" "--a + *z" `shouldParse` (Add (DecL (Var "a")) (Deref (Var "z")))
    it "parses prefix not and equality" $
        parse (pExpr <* eof) "" "a == !a" `shouldParse` (Eq (Var "a") (Not (Var "a")))
    it "parses a function call with an expr as the function" $
        parse (pExpr <* eof) "" "(a+2)(a, b, c)" `shouldParse` (FunCall [Var "a", Var "b", Var "c"] (Add (Var "a") (IntT 2)))
    it "parses an assignment to a string" $
        parse (pExpr <* eof) "" "a = \"testing\"" `shouldParse` (Assign (Var "a") (StringT "testing"))
    it "parses an assignment to a string with escapes" $
        parse (pExpr <* eof) "" "a = \"a\nb\tc\"" `shouldParse` (Assign (Var "a") (StringT "a\nb\tc"))
    it "parses an assignment to a string with escaped quotations" $
        parse (pExpr <* eof) "" "a = \"testing\\\"testing\"" `shouldParse` (Assign (Var "a") (StringT "testing\"testing"))
    it "parses multiple unary operators in the right order" $
        parse (pExpr <* eof) "" "-!x++" `shouldParse` (Neg (Not (IncR (Var "x"))))
 
-- Tests for constant expression evaluation
checkConstExprEval :: Spec
checkConstExprEval = describe "tests for constant expression evaluation" $ do
    it "evaluates simple addition and division" $
        evalConstExpr (Div (Add (IntT 2) (IntT 4)) (IntT 2)) == (Just 3)
    it "evaluates bitwise shifts left and right" $
        evalConstExpr (ShiftR (ShiftL (IntT 1) (IntT 4)) (IntT 3)) == (Just 2)
    it "doesn't evaluate exprs with variables" $
        evalConstExpr (Add (Var "a") (IntT 2)) == Nothing
    it "evaluates equality and bitwise or" $
        evalConstExpr (Eq (BitOr (IntT 7) (IntT 8)) (IntT 15)) == (Just 1)
    it "evaluates logical not" $
        evalConstExpr (Not (Add (IntT 8) (IntT 34))) == (Just 0)

-- Tests for statement parsing (ex: if (x) {a;})
checkStatements :: Spec
checkStatements = describe "tests for statements" $ do
    it "parses a simple assignment expression" $
        parse (pStatement <* eof) "" "a = 2;" `shouldParse` (ExprT (Assign (Var "a") (IntT 2)))
    it "parses a return statement" $ 
        parse (pStatement <* eof) "" "return 4 * 3;" `shouldParse` (Return (Mul (IntT 4) (IntT 3)))
    it "parses a goto statement" $ 
        parse (pStatement <* eof) "" "goto L2;" `shouldParse` (Goto "L2")
    it "parses a label declaration" $
        parse (pStatement <* eof) "" "L2:" `shouldParse` (LabelDec "L2")
    it "parses a ternary, not a label" $
        parse (pStatement <* eof) "" "a?b:c;" `shouldParse` (ExprT (TernIf (Var "a") (Var "b") (Var "c"))) 

parseSpec :: Spec
parseSpec = describe "parser tests" $ do
    checkExprs
    checkConstExprEval
    checkStatements