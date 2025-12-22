module Compiler.Phases.SpecAssignHomes(specAssignHomes) where 

import Test.Hspec

import Compiler.Syntax
import Compiler.Syntax.SampleProgs
import Compiler.Phases

-- Tests for the LangInt.SelInstr (Select instructions)
specAssignHomes :: Spec
specAssignHomes = do
  describe "Tests for module Compiler.Phases.SelectInstr" $ do
    it "pp $ testah prog01" $ do                           
       (pp $ testah prog01) `shouldBe` "    movq  $10, -8(%rbp)\n    negq  -8(%rbp)\n    movq  $42, %rax\n    addq  -8(%rbp), %rax\n    movq  %rax, -16(%rbp)\n    movq  -16(%rbp), %rdi\n    callq print" 
    it "pp $ testah prog02" $ do
       (pp $ testah prog02) `shouldBe` "    movq  $42, -8(%rbp)\n    movq  -8(%rbp), -16(%rbp)\n    movq  -16(%rbp), %rdi\n    callq print" 
    it "pp $ testah prog03" $ do
       (pp $ testah prog03) `shouldBe` "    movq  $10, -8(%rbp)\n    movq  $20, -16(%rbp)\n    movq  $30, -24(%rbp)\n    movq  $40, -32(%rbp)\n    movq  -8(%rbp), %rax\n    subq  -16(%rbp), %rax\n    movq  %rax, -40(%rbp)\n    movq  -24(%rbp), %rax\n    subq  -32(%rbp), %rax\n    movq  %rax, -48(%rbp)\n    movq  -40(%rbp), %rax\n    addq  -48(%rbp), %rax\n    movq  %rax, -56(%rbp)\n    movq  -56(%rbp), %rdi\n    callq print" 
    it "pp $ testah prog04" $ do
       (pp $ testah prog04) `shouldBe` "    movq  $42, %rax\n    subq  $84, %rax\n    movq  %rax, -8(%rbp)\n    movq  $42, %rax\n    subq  $84, %rax\n    movq  %rax, -16(%rbp)\n    movq  -16(%rbp), %rax\n    addq  $25, %rax\n    movq  %rax, -24(%rbp)\n    movq  -8(%rbp), %rax\n    subq  -24(%rbp), %rax\n    movq  %rax, -32(%rbp)\n    movq  -32(%rbp), %rdi\n    callq print"
    it "pp $ testah prog06" $ do
       (pp $ testah prog06) `shouldBe` "    movq  -8(%rbp), %rax\n    subq  -16(%rbp), %rax\n    movq  %rax, -24(%rbp)\n    movq  -32(%rbp), %rax\n    subq  -40(%rbp), %rax\n    movq  %rax, -48(%rbp)\n    movq  -24(%rbp), %rax\n    addq  -48(%rbp), %rax\n    movq  %rax, -56(%rbp)"
    it "pp $ testah prog07" $ do
       (pp $ testah prog07) `shouldBe` "    movq  -8(%rbp), -16(%rbp)\n    negq  -16(%rbp)\n    movq  -24(%rbp), %rax\n    addq  -16(%rbp), %rax\n    movq  %rax, -32(%rbp)\n    movq  -32(%rbp), %rdi\n    callq print" 
 
testah :: SProg ->  ProgAsmV
testah = assignHomes . selectInstr . rco