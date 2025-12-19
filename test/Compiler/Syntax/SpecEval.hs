module Compiler.Syntax.SpecEval (specEval) where

import Test.Hspec
import Compiler.Syntax

-- To redirect stdin see:
-- https://stackoverflow.com/questions/76683594/in-haskell-how-can-i-interact-with-stdin-of-an-io

import System.IO.Silently

-- Run the test in the IO Monad
evaltest :: SStmt -> IO String
evaltest e = do
    output <- capture_ $ evalSStmt e
    pure output

specEval :: Spec
specEval = do
  describe "Tests for module LangInt.Eval" $ do
    it "evaltest testLit01" $ do
        evaltest (testLit01) `shouldReturn` "34\n"
    it "evaltest testNeg01" $ do
        evaltest (testNeg01) `shouldReturn` "-42\n"
    it "evaltest testNeg02" $ do
        evaltest (testNeg02) `shouldReturn` "5\n"
    it "evaltest testAdd01" $ do
        evaltest (testAdd01) `shouldReturn` "42\n"
    it "evaltest testAdd02" $ do
        evaltest (testAdd02) `shouldReturn` "10\n"
    it "evaltest testSub01" $ do
        evaltest (testSub01) `shouldReturn` "-26\n"
    it "evaltest testSub02" $ do
        evaltest (testSub02) `shouldReturn` "-2\n"

testLit01 :: SStmt
testLit01 = SStmtPrint  (SExprInt 34)

testNeg01 :: SStmt
testNeg01 = SStmtPrint  (SExprUOp USub  (SExprInt 42))

testNeg02 :: SStmt
testNeg02 = SStmtPrint  (SExprBinOp Add (SExprInt 8) (SExprUOp USub ((SExprBinOp Add (SExprInt 1) (SExprInt 2)))))

testAdd01 :: SStmt
testAdd01 = SStmtPrint  (SExprBinOp Add (SExprInt 8) (SExprInt 34))

testAdd02 :: SStmt
testAdd02 = SStmtPrint   (SExprBinOp Add (SExprBinOp Add (SExprInt 1) (SExprInt 2)) (SExprBinOp Add (SExprInt 3) (SExprInt 4)))

testSub01 :: SStmt
testSub01 = SStmtPrint   (SExprBinOp Sub (SExprInt 8) (SExprInt 34))

testSub02 :: SStmt
testSub02 = SStmtPrint  (SExprBinOp Add (SExprBinOp Sub (SExprInt 1) (SExprInt 2)) (SExprBinOp Sub (SExprInt 3) (SExprInt 4)))