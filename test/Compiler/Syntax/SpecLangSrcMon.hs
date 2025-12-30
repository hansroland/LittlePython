module Compiler.Syntax.SpecLangSrcMon (specLangSrcMon) where

import Test.Hspec
import Compiler.Syntax

-- Tests for the LangInt.Print interpreters
specLangSrcMon :: Spec
specLangSrcMon = do

  describe "Tests for module LangSrcMon Syntax" $ do
   it "pp atm01" $ do
       (pp atm01) `shouldBe` "5"
   it "pp atm02" $ do
       (pp atm02) `shouldBe` "variable"
   it "pp expr01" $ do
       (pp expr01) `shouldBe` "variable"
   it "pp expr02" $ do
       (pp expr02) `shouldBe` "5 + variable"
   it "pp expr03" $ do
       (pp expr03) `shouldBe` " -variable"
   it "pp expr04" $ do
       (pp expr04) `shouldBe` "call read_int()"
   it "pp stmt01" $ do
       (pp stmt01) `shouldBe` "print_int 5"
   it "pp stmt02" $ do
       (pp stmt02) `shouldBe` "myVar = 5 + variable"

atm01 :: MAtom
atm01 = MAtomInt 5 

atm02 :: MAtom
atm02 = MAtomVar "variable" 

expr01 :: MExpr 
expr01 = MExprAtom atm02

expr02 :: MExpr 
expr02 = MExprBinOp Add atm01 atm02 

expr03 :: MExpr 
expr03 = MExprUOp USub atm02 

expr04 :: MExpr
expr04 = MExprCall "read_int" []

stmt01 :: MStmt 
stmt01 = MStmtCall "print_int" atm01

stmt02 :: MStmt 
stmt02 = MStmtAssign "myVar" expr02
