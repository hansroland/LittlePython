module Compiler.Phases.SpecAssignHomes(specAssignHomes) where 

import Test.Hspec

import Compiler.Syntax
import Compiler.Phases 
import Compiler.Run

-- Tests for Compiler Phases AssignHomes
specAssignHomes :: Spec
specAssignHomes = do
  describe "Tests for module Compiler.Phases.AssignHomes" $ do
    it "testah prog01.lpy" $ do  
       (testah "examples/prog01.lpy") `shouldReturn` "    movq  $10, %rcx\n    negq  %rcx\n    movq  $42, %rcx\n    addq  %rcx, %rcx\n    callq print_int # args:%rcx\n" 
    it "testah prog02" $ do
       (testah "examples/prog02.lpy") `shouldReturn` "    movq  $42, %rcx\n    movq  %rcx, %rcx\n    callq print_int # args:%rcx\n" 
    it "testah prog03" $ do
       (testah "examples/prog03.lpy") `shouldReturn` "    movq  $10, %r8\n    movq  $20, %r9\n    movq  $30, %rdx\n    movq  $40, %rsi\n    movq  %r8, %rdi\n    subq  %r9, %rdi\n    movq  %rdx, %r10\n    subq  %rsi, %r10\n    movq  %rdi, %rcx\n    addq  %r10, %rcx\n    callq print_int # args:%rcx\n" 
    it "testah prog04" $ do
       (testah "examples/prog04.lpy") `shouldReturn` "    movq  $42, %rsi\n    subq  $84, %rsi\n    movq  $42, %rcx\n    subq  $84, %rcx\n    movq  %rcx, %rdi\n    addq  $25, %rdi\n    movq  %rsi, %rdx\n    subq  %rdi, %rdx\n    callq print_int # args:%rdx\n"
    it "testah prog05" $ do
       (testah "examples/prog05.lpy") `shouldReturn` "    %rdx = callq read_int # args:\n    %rsi = callq read_int # args:\n    movq  %rdx, %rcx\n    addq  %rsi, %rcx\n    callq print_int # args:%rcx\n"

testah :: FilePath -> IO String 
testah path = do 
   sprog <- readAndParseSrc (testSettings path)
   let vinstrs = selectInstr $ rco sprog 
   let regmap = assignRegisters vinstrs
   pure $ pp $ assignHomes regmap vinstrs 
