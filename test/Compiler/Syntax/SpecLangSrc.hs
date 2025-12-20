module Compiler.Syntax.SpecLangSrc (specLangSrc) where

import Test.Hspec
import Compiler.Syntax

-- Tests for the pretty-print instances
specLangSrc :: Spec
specLangSrc = do
  describe "Tests for module LangSrc - pretty-print" $ do
    it "testLit01" $ do
        (pp testLit01) `shouldBe` "print 34"
    it "testNeg01" $ do
        (pp testNeg01)`shouldBe` "print -42"
    it "testNeg02" $ do
        (pp testNeg02)`shouldBe` "print (8 + -(1 + 2))"
    it "testAdd01" $ do
        (pp testAdd01) `shouldBe` "print (8 + 34)"
    it "testAdd02" $ do
        (pp testAdd02) `shouldBe` "print ((1 + 2) + (3 + 4))"
    it "testSub01" $ do
        (pp testSub01) `shouldBe` "print (8 - 34)"
    it "testSub02" $ do
        (pp testSub02) `shouldBe` "print ((1 - 2) - (3 - 4))"
    
-- Testcases 
testLit01 :: SStmt
testLit01 = SStmtCall "print" (SExprInt 34)

testNeg01 :: SStmt
testNeg01 = SStmtCall "print" (SExprUOp USub(SExprInt  42))

testNeg02 :: SStmt
testNeg02 = SStmtCall "print" (SExprBinOp Add (SExprInt 8) (SExprUOp USub ((SExprBinOp Add  (SExprInt 1) (SExprInt 2)))))

testAdd01 :: SStmt
testAdd01 = SStmtCall "print" (SExprBinOp Add (SExprInt 8) (SExprInt 34))

testAdd02 :: SStmt
testAdd02 = SStmtCall "print" (SExprBinOp Add (SExprBinOp Add  (SExprInt 1) (SExprInt 2)) (SExprBinOp Add (SExprInt 3) (SExprInt 4)))

testSub01 :: SStmt
testSub01 = SStmtCall "print" (SExprBinOp Sub (SExprInt 8) (SExprInt 34))

testSub02 :: SStmt
testSub02 = SStmtCall "print" (SExprBinOp Sub (SExprBinOp Sub  (SExprInt 1) (SExprInt 2)) (SExprBinOp Sub (SExprInt 3) (SExprInt 4)))