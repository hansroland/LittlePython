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
       (testah "examples/prog01.lpy") `shouldReturn` "    movq  $10, -8(%rbp)\n    negq  -8(%rbp)\n    movq  $42, -16(%rbp)\n    addq  -8(%rbp), -16(%rbp)\n    movq  -16(%rbp), %rdi\n    callq print_int # args:-16(%rbp)\n" 
    it "testah prog02" $ do
       (testah "examples/prog02.lpy") `shouldReturn` "    movq  $42, -8(%rbp)\n    movq  -8(%rbp), -16(%rbp)\n    movq  -16(%rbp), %rdi\n    callq print_int # args:-16(%rbp)\n" 
    it "testah prog03" $ do
       (testah "examples/prog03.lpy") `shouldReturn` "    movq  $10, -8(%rbp)\n    movq  $20, -16(%rbp)\n    movq  $30, -24(%rbp)\n    movq  $40, -32(%rbp)\n    movq  -8(%rbp), -40(%rbp)\n    subq  -16(%rbp), -40(%rbp)\n    movq  -24(%rbp), -48(%rbp)\n    subq  -32(%rbp), -48(%rbp)\n    movq  -40(%rbp), -56(%rbp)\n    addq  -48(%rbp), -56(%rbp)\n    movq  -56(%rbp), %rdi\n    callq print_int # args:-56(%rbp)\n" 
    it "testah prog04" $ do
       (testah "examples/prog04.lpy") `shouldReturn` "    movq  $42, -8(%rbp)\n    subq  $84, -8(%rbp)\n    movq  $42, -16(%rbp)\n    subq  $84, -16(%rbp)\n    movq  -16(%rbp), -24(%rbp)\n    addq  $25, -24(%rbp)\n    movq  -8(%rbp), -32(%rbp)\n    subq  -24(%rbp), -32(%rbp)\n    movq  -32(%rbp), %rdi\n    callq print_int # args:-32(%rbp)\n"
--    it "testah prog05" $ do
--       (testah "examples/prog05.lpy") `shouldReturn` "susi"

testah :: FilePath -> IO String 
testah path = do 
   sprog <- readAndParseSrc (testSettings path)
   let vinstrs = selectInstr $ rco sprog 
   let regmap = assignRegisters vinstrs
   pure $ pp $ assignHomes regmap vinstrs 
