module Compiler.Phases.SpecRco (specRco) where

import Test.Hspec
import Compiler.Syntax
import Compiler.Phases
import Compiler.Syntax.SampleProgs

-- Tests for the LangInt.Rco (Remove complex oprations)
specRco :: Spec
specRco = do
  describe "Tests for module LangSrcMon Rco" $ do
    it "pp $ rco prog01" $ do
       (pp $ rco prog01) `shouldBe` "tmp_0 =  -10\nx = 42 + tmp_0\nprint x" 
    it "pp $ rco prog02" $ do
       (pp $ rco prog02) `shouldBe` "a = 42\nb = a\nprint b" 
    it "pp $ rco prog03" $ do
       (pp $ rco prog03) `shouldBe` "var1 = 10\nvar2 = 20\nvar3 = 30\nvar4 = 40\ntmp_1 = var1 - var2\ntmp_2 = var3 - var4\ntmp_0 = tmp_1 + tmp_2\nprint tmp_0"
    it "pp $ rco prog04" $ do
       (pp $ rco prog04) `shouldBe` "x = 42 - 84\ntmp_0 = 42 - 84\ny = tmp_0 + 25\ntmp_1 = x - y\nprint tmp_1" 
--    it "pp $ rco prog05" $ do
--       (pp $ rco prog05) `shouldBe` "susi"
    it "pp $ rco prog06" $ do
       (pp $ rco prog06) `shouldBe` "tmp_0 = var1 - var2\ntmp_1 = var3 - var4\nz = tmp_0 + tmp_1" 
    it "pp $ rco prog07" $ do
       (pp $ rco prog07) `shouldBe` "tmp_1 =  -y\ntmp_0 = x + tmp_1\nprint tmp_0" 
 