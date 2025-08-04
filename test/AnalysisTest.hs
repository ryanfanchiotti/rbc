module AnalysisTest (analysisSpec) where

import BC.Syntax
import BC.Analysis
import Test.Hspec
import qualified Data.HashSet as S

analysisSpec :: Spec
analysisSpec = describe "tests for analysis" $ do
    checkConstExprEval
    checkIsLValue
    checkExprAnalysis
    checkStatementAnalysis

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
        isLValue var_expr == Right var_expr
    it "determines that deref is an lval" $
        isLValue deref_expr == Right deref_expr
    it "determines that vector indexing is an lval" $
        isLValue idx_expr == Right idx_expr
    it "throws an error on an int" $
        isLValue (IntT 42) == Left "assignment can only be done to vars, derefs, or indexed vecs"
    where
        var_expr = (Var "a")
        deref_expr = (Deref (Div (IntT 2) (IntT 2)))
        idx_expr = (VecIdx (Var "a") (IntT 21))

checkExprAnalysis :: Spec
checkExprAnalysis = describe "tests for analyzing an expr" $ do
    it "performs constant folding on nested expressions" $
        analyzeExpr a_set (Assign (Var "a") (BitOr (Add (IntT 4) (IntT 6)) (IntT 3))) == Right (Assign (Var "a") (IntT 11))
    it "does not perform constant folding on nested expressions with floats" $
        analyzeExpr a_set float_expr == Right float_expr
    it "errors on a bad assignment" $
        analyzeExpr es (Assign (IntT 32) (IntT 32)) == Left "assignment can only be done to vars, derefs, or indexed vecs"
    it "errors on a bad bitwise and plus assignment" $
        analyzeExpr es (AssignBitAnd (IntT 32) (IntT 32)) == Left "assignment can only be done to vars, derefs, or indexed vecs"
    it "errors on use before definition" $
        analyzeExpr es (Assign (Var "z") (IntT 64)) == Left "z used before definition"
    where
        float_expr = (Assign (Var "a") (Mul (Add (FloatT 4) (FloatT 6)) (FloatT 3)))
        es = S.empty
        a_set = S.fromList ["a"]

checkStatementAnalysis :: Spec
checkStatementAnalysis = describe "tests for analyzing a statement" $ do
    it "adds auto names to def vars" $
        analyzeStatement (es, auto_st, es, es) OtherS == 
            Right (S.fromList ["a", "b", "c"], end_auto_st, es, es)
    it "errors on double def in auto" $
        analyzeStatement (S.fromList ["a"], auto_st, es, es) OtherS == 
            Left "auto vars a already defined"
    it "adds extern names to def vars" $
        analyzeStatement (es, (Extern num_lst), es, es) SwitchS == 
            Right (S.fromList num_lst, Extern num_lst, es, es)
    it "errors on double def in extern" $
        analyzeStatement (S.fromList num_lst, (Extern num_lst), es, es) SwitchS == 
            Left "extern vars 1, 22, 333, 4444 already defined"
    it "adds label name to label list" $
        analyzeStatement (es, (LabelDec "hello"), es, es) OtherS == 
            Right (es, LabelDec "hello", S.fromList ["hello"], es)
    it "errors on double def in label" $
        analyzeStatement (S.fromList ["hello"], (LabelDec "hello"), es, es) OtherS ==
            Left "label hello already defined"
    it "adds goto name to goto list" $
        analyzeStatement (es, (Goto "hello"), es, es) OtherS == 
            Right (es, Goto "hello", es, S.fromList ["hello"])
    it "errors on case with non const expr" $
        analyzeStatement (es, (Case (StringT "hello")), es, es) SwitchS ==
            Left "case expr is not constant"
    it "deals with compound statements correctly" $
        analyzeStatement (S.fromList ["a"], cmpd, es, es) SwitchS ==
            Right (S.fromList ["a"], cmpd_after, S.fromList ["heaven"], es)
    where
        auto_st = Auto [("a", Nothing), ("b", Just (Add (IntT 23) (IntT 23))), ("c", Nothing)]
        end_auto_st = Auto [("a", Nothing), ("b", Just (IntT 46)), ("c", Nothing)]

        num_lst = ["1", "22", "333", "4444"]
        es = S.empty

        cmpd = (Compound 
                [ Case (Add (IntT 1) (IntT 3)), 
                  ExprT (Assign (Var "a") (IntT 4)), 
                  Case (IntT 3), 
                  ExprT (Assign (Var "a") (IntT 5)),
                  LabelDec "heaven",
                  Auto [("hello", Nothing)],
                  Return Nothing,
                  ExprT (Assign (Var "a") (IntT 5)) ]
            )
        
        cmpd_after = (Compound 
                [ Case (IntT 4), 
                  ExprT (Assign (Var "a") (IntT 4)), 
                  Case (IntT 3), 
                  ExprT (Assign (Var "a") (IntT 5)),
                  LabelDec "heaven",
                  Auto [("hello", Nothing)],
                  Return Nothing ]
            )
