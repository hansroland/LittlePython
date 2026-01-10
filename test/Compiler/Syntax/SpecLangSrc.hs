module Compiler.Syntax.SpecLangSrc (specLangSrc) where

import Test.Hspec
import Compiler.Syntax
import Compiler.Phases

parsePrint :: String -> String
parsePrint = pp . parseLpy ""

-- Test Parser and PrettyPrint (PP)
specLangSrc :: Spec
specLangSrc = do
  describe "Tests for module LangSrc - pretty-print" $ do
    it "testLit01" $ do
        (parsePrint testLit01) `shouldBe` "print (34)"       
    it "testNeg01" $ do
        (parsePrint testNeg01)`shouldBe` "print (-42)"
    it "testNeg02" $ do
        (parsePrint testNeg02)`shouldBe` "print (8 + -(1 + 2))"
    it "testNeg03" $ do
        (parsePrint testNeg03)`shouldBe` "print (-34)"
    it "testAdd01" $ do
        (parsePrint testAdd01) `shouldBe` "print (8 + 34)"
    it "testAdd02" $ do
        (parsePrint testAdd02) `shouldBe` "print ((1 + 2) + (3 + getInt()))"
    it "testSub01" $ do
        (parsePrint testSub01) `shouldBe` "print (8 - 34)"
    it "testSub02" $ do
        (parsePrint testSub02) `shouldBe`  "print ((1 - -2) - (-3 - 4))"

-- Testcases 
testLit01 :: String
testLit01 = "print (34)"

testNeg01 :: String
testNeg01 = "print(-(42))"

testNeg02 :: String
testNeg02 = "print (8 + (-(1 + 2)))"

testNeg03 :: String 
testNeg03 = "print (-34))"

testAdd01 :: String
testAdd01 = "print (8 + 34)"

testAdd02 :: String
testAdd02 = "print ((1 + 2) + (3 + getInt()))"

testSub01 :: String
testSub01 = "print (8 - 34)"

testSub02 :: String
testSub02 = "print ((1 - -2) - (-3 - 4))"
