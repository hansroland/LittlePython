module Compiler.Syntax.SpecEval (specEval) where

import Test.Hspec
import Utils
import Compiler.Syntax
import Compiler.Syntax.SampleProgs


evalProgWith :: SProg -> String -> String -> IO ()  --includes get_int
evalProgWith sprog inp res = do 
    out <- runWithInput (evalSProg sprog) inp
    shouldContain out res

evalStmt :: SStmt -> String  -> IO ()           -- without get_int
evalStmt stmt out = do
    evalProgWith (SProg [stmt]) "" out

specEval :: Spec
specEval = do
  describe "Tests for module LangInt.Eval" $ do
    it "evalStmt testLit01" $ do
        evalStmt testLit01 "34\n"
    it "evalStmt testNeg01" $ do
        evalStmt (testNeg01) "-42\n"
    it "evalStmt testNeg02" $ do
        evalStmt (testNeg02) "5\n"
    it "evalStmt testAdd01" $ do
        evalStmt (testAdd01) "42\n"
    it "evalStmt testAdd02" $ do
        evalStmt (testAdd02) "10\n"
    it "evalStmt testSub01" $ do
        evalStmt (testSub01) "-26\n"
    it "evalStmt testSub02" $ do
        evalStmt (testSub02) "-2\n"

    it "evalProgWith prog01" $ do
        evalProgWith prog01 "" "32\n"

    it "evalProgWith prog02" $ do
        evalProgWith prog02 "" "42\n"

    it "evalProgWith prog03" $ do
        evalProgWith prog03 "" "-20\n"

    it "evalProgWith prog04" $ do
        evalProgWith prog04 "" "-25\n"

    it "evalProgWith (with input) prog08" $ do 
        evalProgWith prog08 "22\n53" "-31\n" 

testLit01 :: SStmt
testLit01 = SStmtCall "print"  (SExprInt 34)

testNeg01 :: SStmt
testNeg01 = SStmtCall "print"  (SExprUOp USub  (SExprInt 42))

testNeg02 :: SStmt
testNeg02 = SStmtCall "print"  (SExprBinOp Add (SExprInt 8) (SExprUOp USub ((SExprBinOp Add (SExprInt 1) (SExprInt 2)))))

testAdd01 :: SStmt
testAdd01 = SStmtCall "print"  (SExprBinOp Add (SExprInt 8) (SExprInt 34))

testAdd02 :: SStmt
testAdd02 = SStmtCall "print"   (SExprBinOp Add (SExprBinOp Add (SExprInt 1) (SExprInt 2)) (SExprBinOp Add (SExprInt 3) (SExprInt 4)))

testSub01 :: SStmt
testSub01 = SStmtCall "print"   (SExprBinOp Sub (SExprInt 8) (SExprInt 34))

testSub02 :: SStmt
testSub02 = SStmtCall "print"  (SExprBinOp Add (SExprBinOp Sub (SExprInt 1) (SExprInt 2)) (SExprBinOp Sub (SExprInt 3) (SExprInt 4)))
