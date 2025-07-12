module AnalysisTest (analysisSpec) where

import BC.Syntax
import BC.Analysis
import Test.Hspec

analysisSpec :: Spec
analysisSpec = describe "tests for analysis" $ do
    checkConstExprEval
    checkIsLValue
    checkExprAnalysis

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

checkIsLValue :: Spec
checkIsLValue = describe "tests for lvalue checks" $ do
    it "determines that var is an lval" $
        isLValue varExpr == Right varExpr
    it "determines that deref is an lval" $
        isLValue derefExpr == Right derefExpr
    it "determines that vector indexing is an lval" $
        isLValue idxExpr == Right idxExpr
    it "throws an error on an int" $
        isLValue (IntT 42) == Left "assignment can only be done to vars, derefs, or indexed vecs"
    where
        varExpr = (Var "a")
        derefExpr = (Deref (Div (IntT 2) (IntT 2)))
        idxExpr = (VecIdx (Var "a") (IntT 21))

checkExprAnalysis :: Spec
checkExprAnalysis = describe "tests for analyze expr" $ do
    it "performs constant folding on nested expressions" $
        analyzeExpr (Assign (Var "a") (BitOr (Add (IntT 4) (IntT 6)) (IntT 3))) == Right (Assign (Var "a") (IntT 11))
    it "errors on a bad assignment" $
        analyzeExpr (Assign (IntT 32) (IntT 32)) == Left "assignment can only be done to vars, derefs, or indexed vecs"
