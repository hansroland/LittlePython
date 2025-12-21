module Compiler.Syntax.SpecEval (specEval) where

import Test.Hspec
import Compiler.Syntax
import Compiler.Syntax.SampleProgs

-- To redirect stdin see:
-- https://stackoverflow.com/questions/76683594/in-haskell-how-can-i-interact-with-stdin-of-an-io

import System.IO.Silently

-- Run the test in the IO Monad
evalstmt :: SStmt -> IO String
evalstmt e = do
    output <- capture_ $ evalSProg $ SProg [e]
    pure output

evalprog :: SProg -> IO String 
evalprog p = do 
   output <- capture_ $ evalSProg p
   pure output

specEval :: Spec
specEval = do
  describe "Tests for module LangInt.Eval" $ do
    it "evalstmt testLit01" $ do
        evalstmt (testLit01) `shouldReturn` "34\n"
    it "evalstmt testNeg01" $ do
        evalstmt (testNeg01) `shouldReturn` "-42\n"
    it "evalstmt testNeg02" $ do
        evalstmt (testNeg02) `shouldReturn` "5\n"
    it "evalstmt testAdd01" $ do
        evalstmt (testAdd01) `shouldReturn` "42\n"
    it "evalstmt testAdd02" $ do
        evalstmt (testAdd02) `shouldReturn` "10\n"
    it "evalstmt testSub01" $ do
        evalstmt (testSub01) `shouldReturn` "-26\n"
    it "evalstmt testSub02" $ do
        evalstmt (testSub02) `shouldReturn` "-2\n"

    it "evalprog prog01" $ do
        evalprog prog01  `shouldReturn` "32\n"
    it "evalprog prog02" $ do
        evalprog prog02  `shouldReturn` "42\n"
    it "evalprog prog03" $ do
        evalprog prog03  `shouldReturn` "-20\n"
    it "evalprog prog04" $ do
        evalprog prog04  `shouldReturn` "-25\n"

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



