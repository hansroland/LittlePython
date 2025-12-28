module Compiler.Syntax.SpecLangXAsm86 (specLangXAsm86) where

import Test.Hspec
import Compiler.Syntax

-- Tests for the AsmX86 base language
specLangXAsm86 :: Spec
specLangXAsm86 = do

  describe "Tests for module AsmX86Int Syntax" $ do
   -- Register
   it "pp Rbp" $ do
       (pp Rbp) `shouldBe` "%rbp"
       writeFile "susi.txt" "susi"
   -- Destination
   it "pp dst01" $ do
       pp dst01 `shouldBe` "%rax"
   it "pp dst02" $ do
       pp dst02 `shouldBe` "42(%rax)"
   -- Source
   it "pp src01" $ do
       pp src01 `shouldBe` "%rax"
   it "pp src02" $ do
       pp src02 `shouldBe` "42(%rbp)"
   it "pp src03" $ do
       pp src03 `shouldBe` "$25"
   -- Instructions
   it "pp instr01" $ do 
      pp instr01 `shouldBe` "    addq  %rax, %rax"
   it "pp instr02" $ do 
      pp instr02 `shouldBe` "    negq  42(%rax)"
   it "pp instr03" $ do 
      pp instr03 `shouldBe` "    retq"
   it "pp vinstr01" $ do 
      pp vinstr01 `shouldBe` "    movq  varA, varB"
   it "pp glob" $ do 
      pp glob `shouldBe`  "    .globl main"
   it "pp labl" $ do 
      pp labl `shouldBe`  "main:"


dst01, dst02 :: AsmIOp 
dst01 = IReg Rax 
dst02 = IMem 42 Rax

src01, src02, src03 :: AsmIOp 
src01 = IReg Rax
src02 = IMem 42 Rbp
src03 = IImm 25

vsrc01 :: AsmVOp
vsrc01 = VVar "varA"

vdst01 :: AsmVOp 
vdst01 = VVar "varB"

instr01, instr02, instr03 :: InstrInt
instr01 = Instr2 Addq src01 dst01
instr02 = Instr1 Negq dst02 
instr03 = Instr0 Retq 

vinstr01 :: InstrVar 
vinstr01 = Instr2 Movq vsrc01 vdst01

glob :: InstrInt 
glob = InstrGlob "main"

labl :: InstrInt
labl = InstrLabl "main"
