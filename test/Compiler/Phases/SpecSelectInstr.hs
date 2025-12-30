module Compiler.Phases.SpecSelectInstr (specSelInstr) where

import Test.Hspec
import Compiler.Syntax 
import Compiler.Syntax.SampleProgs
import Compiler.Phases

-- Tests for the LangInt.SelInstr (Select instructions)
specSelInstr :: Spec
specSelInstr = do
  describe "Tests for module Compiler.Phases.SelectInstr" $ do
    it "pp $ testsi prog01" $ do                           
       (pp $ testsi prog01) `shouldBe` "    movq  $10, tmp_0\n    negq  tmp_0\n    movq  $42, %rax\n    addq  tmp_0, %rax\n    movq  %rax, x\n    movq  x, %rdi\n    callq print_int" 
    it "pp $ testsi prog02" $ do
       (pp $ testsi prog02) `shouldBe` "    movq  $42, a\n    movq  a, b\n    movq  b, %rdi\n    callq print_int" 
    it "pp $ testsi prog03" $ do
       (pp $ testsi prog03) `shouldBe` "    movq  $10, var1\n    movq  $20, var2\n    movq  $30, var3\n    movq  $40, var4\n    movq  var1, %rax\n    subq  var2, %rax\n    movq  %rax, tmp_1\n    movq  var3, %rax\n    subq  var4, %rax\n    movq  %rax, tmp_2\n    movq  tmp_1, %rax\n    addq  tmp_2, %rax\n    movq  %rax, tmp_0\n    movq  tmp_0, %rdi\n    callq print_int" 
    it "pp $ testsi prog04" $ do
       (pp $ testsi prog04) `shouldBe` "    movq  $42, %rax\n    subq  $84, %rax\n    movq  %rax, x\n    movq  $42, %rax\n    subq  $84, %rax\n    movq  %rax, tmp_0\n    movq  tmp_0, %rax\n    addq  $25, %rax\n    movq  %rax, y\n    movq  x, %rax\n    subq  y, %rax\n    movq  %rax, tmp_1\n    movq  tmp_1, %rdi\n    callq print_int" 
    it "pp $ testsi prog06" $ do
       (pp $ testsi prog06) `shouldBe` "    movq  var1, %rax\n    subq  var2, %rax\n    movq  %rax, tmp_0\n    movq  var3, %rax\n    subq  var4, %rax\n    movq  %rax, tmp_1\n    movq  tmp_0, %rax\n    addq  tmp_1, %rax\n    movq  %rax, z"
    it "pp $ testsi prog07" $ do
       (pp $ testsi prog07) `shouldBe` "    movq  y, tmp_1\n    negq  tmp_1\n    movq  x, %rax\n    addq  tmp_1, %rax\n    movq  %rax, tmp_0\n    movq  tmp_0, %rdi\n    callq print_int" 
 
testsi :: SProg ->  ProgAsmV
testsi p = selectInstr $ rco p