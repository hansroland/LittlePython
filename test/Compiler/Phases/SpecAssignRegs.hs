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
    let instrAdd = Instr2 Addq w z
    it "testReadsAdd" $ do
       pp (rOps instrAdd) `shouldBe` "{w z}"
    it "testWritesAdd" $ do
       pp (wOps instrAdd) `shouldBe` "{z}"

    let instrMov = Instr2 Movq w z
    it "testReadsMove" $ do
       pp (rOps instrMov) `shouldBe` "{w}"
    it "testWritesMov" $ do
       pp (wOps instrMov) `shouldBe` "{z}"

    let edges01 = [(v,w), (w,x), (x,y), (y,v)]
    let vars01 = [v,w,x,y]
    it "color graph square" $ do 
       (colorGraph edges01 vars01) `shouldBe` [(v,0),  (x,1),  (w,0),  (y,1)]

    let edges02 = edges01 <> [(v,z), (w,z), (x,z), (y,z)]
    let vars02 = z : vars01
    it "color graph pyramid" $ do 
       (colorGraph edges02 vars02) `shouldBe` [(v,0),  (x,1),  (z,2),  (w,3),  (y,4)]

    it "testul book43" $ do  
       (testul book43) `shouldBe` "{}\n{a}\n{a}\n{c}\n{b c}" 
    it "testul book44" $ do  
       (testul book44) `shouldBe` "{}\n{v}\n{v w}\n{w x}\n{w x}\n{w x y}\n{w y z}\n{y z}\n{tmp0 z}\n{tmp0 z}\n{tmp0 tmp1}\n{tmp1}" 

    it "testcp book44" $ do  
       (testcp book44) `shouldBe` "[(w, v), (x, v), (x, w), (y, w), (y, x), (z, w), (z, x), (z, y), (tmp0, y), (tmp0, z), (tmp1, tmp0), (tmp1, z)]" 

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
--    , Instr2 Movq tmp1 (VReg Rdi)
    , InstrCall "print_int" [tmp1]
    ]


-- Variables to create tests
v, w, x, y, z, tmp0, tmp1, n1, n42, n7 :: AsmVOp 
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
