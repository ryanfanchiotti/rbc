module ParserTest (parseSpec) where

import Control.Applicative hiding (some)
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char
import BC.Parser
import BC.Syntax

checkExprs :: Spec
checkExprs = describe "tests for exprs (which roughly correspond to an rvalue)" $ do
    it "parses simple int" $
        parse (pExpr <* eof) "" "1" `shouldParse` (IntT 1)
    it "parses a character as an int" $
        parse (pExpr <* eof) "" "'a'" `shouldParse` (IntT 97)
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

checkStatements :: Spec
checkStatements = describe "tests for statement parsing (ex: if (x) {a;})" $ do
    it "parses a simple assignment expression" $
        parse (pStatement <* eof) "" "a = 2;" `shouldParse` (ExprT (Assign (Var "a") (IntT 2)))
    it "parses a return statement" $
        parse (pStatement <* eof) "" "return 4 * 3;" `shouldParse` (Return (Just (Mul (IntT 4) (IntT 3))))
    it "parses a goto statement" $
        parse (pStatement <* eof) "" "goto L2;" `shouldParse` (Goto "L2")
    it "parses a label declaration" $
        parse (pStatement <* eof) "" "L2:" `shouldParse` (LabelDec "L2")
    it "parses a ternary, not a label" $
        parse (pStatement <* eof) "" "a?b:c;" `shouldParse` (ExprT (TernIf (Var "a") (Var "b") (Var "c")))
    it "parses a simple compound statement" $
        parse (pStatement <* eof) "" "{L2: a; goto L2;}" `shouldParse` (Compound [LabelDec "L2", ExprT (Var "a"), Goto "L2"])
    it "parses an extern declaration list" $
        parse (pStatement <* eof) "" "extern a, b, c;" `shouldParse` (Extern ["a", "b", "c"])
    it "doesn't parse an empty extern declaration list" $
        parse (pStatement <* eof) "" `shouldFailOn` "extern;"
    it "parses a case statement" $
        parse (pStatement <* eof) "" "case 3:" `shouldParse` (Case (IntT 3))
    it "parses a switch statement with cases" $
        parse (pStatement <* eof) "" "switch (3) {case 1: a = 4; case 3: a = 5;}" `shouldParse`
        (Switch (IntT 3) 
            (Compound 
                [ Case (IntT 1), 
                  ExprT (Assign (Var "a") (IntT 4)), 
                  Case (IntT 3), 
                  ExprT (Assign (Var "a") (IntT 5)) ]
            )
        )
    it "parses an if else statement" $
        parse (pStatement <* eof) "" "if (3 > 2) return 1; else return 2;" `shouldParse`
        (IfElse (Gt (IntT 3) (IntT 2)) (Return (Just (IntT 1))) (Return (Just (IntT 2))))
    it "parses a regular if statement" $
        parse (pStatement <* eof) "" "if (1 & 2) {a += 1; return 4;}" `shouldParse`
        (If (BitAnd (IntT 1) (IntT 2)) (Compound [ExprT (AssignAdd (Var "a") (IntT 1)), Return (Just (IntT 4))]))
    it "parses a while loop" $
        parse (pStatement <* eof) "" "while (a) a -= 1;" `shouldParse` (While (Var "a") (ExprT (AssignSub (Var "a") (IntT 1))))
    it "parses an auto declaration" $
        parse (pStatement <* eof) "" "auto a, b[23], c;" `shouldParse`
        (Auto [("a", Nothing), ("b", Just (IntT 23)), ("c", Nothing)])

checkDefs :: Spec
checkDefs = describe "tests for definition parsing (functions and globals)" $ do
    it "parses a simple function definition" $
        parse (pDef <* eof) "" "func(test, aaa, b) {z = 4; printf(aaa);}" `shouldParse`
        (Func ("func") (["test", "aaa", "b"]) 
            (Compound
                [ ExprT (Assign (Var "z") (IntT 4)), 
                  ExprT (FunCall ([Var "aaa"]) (Var "printf"))
                ]
            )
        )
    it "parses a vector definition with an initialization list" $
        parse (pDef <* eof) "" "vec[3] {1, 2, 3};" `shouldParse`
        (GlobalVec "vec" (Just (IntT 3)) (Just ([IntT 1, IntT 2, IntT 3])))
    it "parses a global def with a value" $
        parse (pDef <* eof) "" "glob 1;" `shouldParse` (Global "glob" (Just (IntT 1)))
    it "parses a global def without a value" $
        parse (pDef <* eof) "" "glob;" `shouldParse` (Global "glob" Nothing)

checkProgs :: Spec
checkProgs = describe "tests for program parsing" $ do
    it "parses a simple program" $
        parse (pProg <* eof) "" (unlines [
            "myglob 1;",
            "myvec[];",
            "main(a,b,c) {",
            "   auto d;",
            "   d = a + b + c;",
            "   printf(\"Hello World %d\", d);",
            "}"
        ]) `shouldParse` [
            Global "myglob" (Just (IntT 1)), 
            GlobalVec "myvec" Nothing Nothing, 
            Func "main" ["a", "b", "c"] 
            (Compound [
                Auto [("d", Nothing)],
                ExprT (Assign (Var "d") (Add (Add (Var "a") (Var "b")) (Var "c"))),
                ExprT (FunCall [StringT "Hello World %d", Var "d"] (Var "printf"))
            ])
        ]
    it "parses a program with chained if else" $
        parse (pProg <* eof) "" (unlines [
            "main () {",
            "   if (1) 1;",
            "   else if (2) 2;",
            "   else if (3) 3;",
            "   else 4;",
            "   5;",
            "}"
        ]) `shouldParse` [
            Func "main" [] 
            (Compound [
                IfElse (IntT 1) 
                    (ExprT (IntT 1)) 
                    (IfElse (IntT 2) 
                        (ExprT (IntT 2)) 
                        (IfElse (IntT 3) 
                            (ExprT (IntT 3)) 
                            (ExprT (IntT 4)))), 
                ExprT (IntT 5)])
        ]

parseSpec :: Spec
parseSpec = describe "parser tests" $ do
    checkExprs
    checkStatements
    checkDefs
    checkProgs
