module Compiler.Phases.SpecRco (specRco) where

import Test.Hspec
import Compiler.Syntax
import Compiler.Phases
import Compiler.Run

-- Tests for the LangInt.Rco (Remove complex oprations)
specRco :: Spec
specRco = do
  describe "Tests for module LangSrcMon Rco" $ do
    it "testrco examples/prog01.lpy" $ do
       (testrco "examples/prog01.lpy") `shouldReturn` "tmp_0 =  -10\nx = 42 + tmp_0\nprint x" 
    it "testrco examples/prog02.lpy" $ do
       (testrco "examples/prog02.lpy") `shouldReturn` "a = 42\nb = a\nprint b" 
    it "testrco examples/prog03.lpy" $ do
       (testrco "examples/prog03.lpy") `shouldReturn` "var1 = 10\nvar2 = 20\nvar3 = 30\nvar4 = 40\ntmp_1 = var1 - var2\ntmp_2 = var3 - var4\ntmp_0 = tmp_1 + tmp_2\nprint tmp_0"
    it "testrco examples/prog04.lpy" $ do
       (testrco "examples/prog04.lpy") `shouldReturn` "x = 42 - 84\ntmp_0 = 42 - 84\ny = tmp_0 + 25\ntmp_1 = x - y\nprint tmp_1" 
    it "testrco examples/prog05.lpy" $ do
       (testrco "examples/prog05.lpy") `shouldReturn` "tmp_1 = getInt()\ntmp_2 = getInt()\ntmp_0 = tmp_1 + tmp_2\nprint tmp_0" 
--    it "pp $ rco prog06" $ do
--       (pp $ rco prog06) `shouldBe` "tmp_0 = var1 - var2\ntmp_1 = var3 - var4\nz = tmp_0 + tmp_1" 
--    it "pp $ rco prog07" $ do
--        (pp $ rco prog07) `shouldBe` "tmp_1 =  -y\ntmp_0 = x + tmp_1\nprint tmp_0" 

testrco :: FilePath -> IO String 
testrco path = do 
   sprog <- readAndParseSrc (testSettings path)
   pure $ pp $ rco sprog 
