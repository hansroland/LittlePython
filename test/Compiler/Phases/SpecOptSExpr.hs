module Compiler.Phases.SpecOptSExpr (specOptSExpr) where

import Test.Hspec
import Compiler.Syntax
import Compiler.Phases

runOptConst :: SExpr -> String
runOptConst = show . optimizeSExpr

specOptSExpr :: Spec
specOptSExpr = do
  describe "Tests for module OptSExpr" $ do
    it "runOptConst ex00" $ do
        (runOptConst ex00) `shouldBe` "SExprInt 29"
    it "runOptConst ex01" $ do
        (runOptConst ex01) `shouldBe` 
            "SExprBinOp Add (SExprInt 3) (SExprBinOp Add (SExprInt 3) (SExprFunc \"getInt\"))"
    it "runOptConst ex02" $ do
        (runOptConst ex02) `shouldBe` 
            "SExprBinOp Add (SExprFunc \"getInt\") (SExprUOp USub (SExprInt 8))"
    it "runOptConst ex03" $ do
        (runOptConst ex03) `shouldBe` 
            "SExprBinOp Add (SExprUOp USub (SExprInt 3)) (SExprUOp USub (SExprBinOp Add (SExprInt 3) (SExprFunc \"getInt\")))"


-- Parts of the syntax
ex00 :: SExpr
ex00 = SExprBinOp Add (SExprBinOp Add (SExprInt 10) (SExprInt 20)) (SExprBinOp Sub (SExprInt 3) (SExprInt 4))

ex01 :: SExpr
ex01 = SExprBinOp Add (SExprBinOp Add (SExprInt 1) (SExprInt 2)) (SExprBinOp Add (SExprInt 3) (SExprFunc "getInt"))

ex02 :: SExpr
ex02 = SExprBinOp Add (SExprFunc "getInt") (SExprUOp USub (SExprBinOp Add (SExprInt 5) (SExprInt 3)))

ex03 :: SExpr
ex03 = SExprUOp USub (SExprBinOp Add (SExprBinOp Add (SExprInt 1) (SExprInt 2)) (SExprBinOp Add (SExprInt 3) (SExprFunc "getInt")))
