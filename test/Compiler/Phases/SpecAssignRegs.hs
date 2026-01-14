{-# LANGUAGE ScopedTypeVariables #-}

module Compiler.Phases.SpecAssignRegs(specAssignRegs) where 

import Test.Hspec
import Compiler.Syntax
import Compiler.Phases 

-- Function to test uncoverLive
testul :: [InstrVar] -> String 
testul sample = pp $ snd <$> (uncoverLive sample)

-- Function to test createEdgePairs ::  (InstrVar, [AsmVOp]) -> [(AsmVOp, AsmVOp)] 
-- Here we first run uncoverLive and then createEdgePairs. 
--    If the tests with uncoverLives fail, then those with createEdgePairs will fail too!
testcp :: [InstrVar] -> String 
testcp sample = pp $ edgePairs sample

specAssignRegs :: Spec
specAssignRegs = do
  describe "Tests for module Compiler.Phases.AssignRegs" $ do
    it "testul book43" $ do  
       (testul book43) `shouldBe` "{a}\n{a}\n{c}\n{b c}\n{}" 
    it "testul book44" $ do  
       (testul book44) `shouldBe` "{v}\n{v w}\n{w x}\n{w x}\n{w x y}\n{w y z}\n{y z}\n{tmp0 z}\n{tmp0 z}\n{tmp0 tmp1}\n{tmp1}\n{%rdi}\n{}" 

    it "testcp book44" $ do  
       (testcp book44) `shouldBe` "[(w, v), (x, w), (y, w), (z, w), (z, y), (tmp0, z), (tmp1, tmp0)]" 

-- Example from figure 4.3
-- Note. We cannot send the example of the book through the
-- selectInstructions function, because it already uses the %rax
-- register for the addition instead of the variables !
book43 :: [InstrVar]
book43 = 
  let i5  = VImm 5
      i10 = VImm 10
      i30 = VImm 30
      a = VVar "a" 
      b = VVar "b"
      c = VVar "c"
  in
    [ Instr2 Movq i5 a 
    , Instr2 Movq i30 b 
    , Instr2 Movq a c 
    , Instr2 Movq i10 b
    , Instr2 Addq b c
    ]

book44 :: [InstrVar]
book44 = 
  let v, w, x, y, z, tmp0, tmp1, n1, n42, n7 :: AsmVOp 
      v = VVar "v"
      w = VVar "w"
      x = VVar "x" 
      y = VVar "y"
      z = VVar "z"
      tmp0 = VVar "tmp0"
      tmp1 = VVar "tmp1"
      n1 = VImm 1
      n7 = VImm 7
      n42 = VImm 42
  in 
    [ Instr2 Movq n1  v
    , Instr2 Movq n42 w
    , Instr2 Movq v x
    , Instr2 Addq n7 x
    , Instr2 Movq x y
    , Instr2 Movq x z
    , Instr2 Addq w z
    , Instr2 Movq y tmp0
    , Instr1 Negq tmp0
    , Instr2 Movq z tmp1
    , Instr2 Addq tmp0 tmp1
    , Instr2 Movq tmp1 (VReg Rdi)
    , Instr0 (Callq "print_int" 1)
    ]
