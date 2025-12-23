-- Testcases for the module Compiler.Phases.PatchInstr

module Compiler.Phases.SpecPatchInstr( specPatchInstr) where 

import Test.Hspec

import Compiler.Syntax 
import Compiler.Phases

specPatchInstr :: Spec 
specPatchInstr = do
  describe "Tests for module Compiler.Phases.PatchInstr" $ do
    -- Test two memory addresses in one instruction
    it "pp $ patchInstr prog01" $ do                           
       (pp $ patchInstr prog01) `shouldBe` "    movq  -8(%rbp), %rax\n    subq  %rax, -16(%rbp)" 
    -- Test two registers in one instruction
    it "pp $ patchInstr prog02" $ do
       (pp $ patchInstr prog02) `shouldBe` "    addq  %rbp, %rax" 
    -- Test positive big immediate number and memory loc in one instruction   
    it "pp $ patchInstr prog03" $ do
       (pp $ patchInstr prog03) `shouldBe` "    movq  $2147483649, %rax\n    subq  -16(%rax), %rax" 
    --  Test negative "big" number and memory loc in one instruction
    it "pp $ patchInstr prog04" $ do
       (pp $ patchInstr prog04) `shouldBe` "    movq  $-2147483649, %rax\n    movq  -16(%rax), %rax" 
    -- Test normal instructions
    it "pp $ patchInstr prog05" $ do
       (pp $ patchInstr prog05) `shouldBe` "    addq  $42, %rax" 
    it "pp $ patchInstr prog06" $ do
       (pp $ patchInstr prog06) `shouldBe` "    negq  %rax" 


prog01 :: ProgAsmI
prog01 = ProgAsmI 16  [Instr2 Subq (IMem (-8) Rbp) (IMem (-16) Rbp)]

prog02 :: ProgAsmI 
prog02 = ProgAsmI 32 [Instr2 Addq (IReg Rbp) (IReg Rax)]

prog03 :: ProgAsmI
prog03 = ProgAsmI 32 [Instr2 Subq (IImm bigNum) (IMem (-16) Rax)]

prog04 :: ProgAsmI
prog04 = ProgAsmI 32 [Instr2 Movq (IImm (-bigNum)) (IMem (-16) Rax)]

prog05 :: ProgAsmI
prog05 = ProgAsmI 32 [Instr2 Addq (IImm 42) (IReg Rax)]

prog06 :: ProgAsmI
prog06 = ProgAsmI 32 [Instr1 Negq (IReg Rax)]

bigNum :: Int 
bigNum =  1 + ((2::Int) ^ (31::Int))