module Compiler.Syntax.SpecEval (specEval) where

import Test.Hspec
import Compiler.Run
import Utils
import Compiler.Syntax
import System.FilePath

evalProgWith :: FilePath -> String -> IO String  --includes get_int
evalProgWith path inp = do 
    sprog <- readAndParseSrc (testSettings $ "examples" </> path <.> "lpy")
    out <- runWithInput (evalSProg sprog) inp
    pure out

evalStmt :: SStmt -> IO String               -- without get_int
evalStmt stmt = do
    runWithInput (evalSProg (SProg [stmt])) ""

specEval :: Spec
specEval = do
  describe "Tests for module LangInt.Eval" $ do
    it "evalStmt testLit01" $ do
        evalStmt testLit01 `shouldReturn` "34\n"
    it "evalStmt testNeg01" $ do
        evalStmt (testNeg01) `shouldReturn` "-42\n"
    it "evalStmt testNeg02" $ do
        evalStmt (testNeg02) `shouldReturn` "5\n"
    it "evalStmt testAdd01" $ do
        evalStmt (testAdd01) `shouldReturn` "42\n"
    it "evalStmt testAdd02" $ do
        evalStmt (testAdd02) `shouldReturn` "10\n"
    it "evalStmt testSub01" $ do
        evalStmt (testSub01) `shouldReturn` "-26\n"
    it "evalStmt testSub02" $ do
        evalStmt (testSub02) `shouldReturn` "-2\n"

    it "evalProgWith prog01" $ do
        evalProgWith "prog01" "" `shouldReturn` "32\n"

    it "evalProgWith prog02" $ do
        evalProgWith "prog02" "" `shouldReturn` "42\n"

    it "evalProgWith prog03" $ do
        evalProgWith "prog03" "" `shouldReturn` "-20\n"

    it "evalProgWith prog04" $ do
        evalProgWith "prog04" "" `shouldReturn` "-25\n"

    it "evalProgWith (with input) prog05" $ do 
        evalProgWith "prog05" "30\n12\n" `shouldReturn` "42\n" 

    it "evalProgWith prog06" $ do
        evalProgWith "prog06" "" `shouldReturn` "32\n45\n"
    
    it "compileAndRun prog06" $ do 
        compileAndRun "prog06" `shouldReturn` "32\n45\n"

testLit01 :: SStmt
testLit01 = SStmtCall "print"  (SExprInt 34)

testNeg01 :: SStmt
testNeg01 = SStmtCall "print"  (SExprUOp USub  (SExprInt 42))

testNeg02 :: SStmt
testNeg02 = SStmtCall "print"  (SExprBinOp Add (SExprInt 8) (SExprUOp USub ((SExprBinOp Add (SExprInt 1) (SExprInt 2)))))

testAdd01 :: SStmt
testAdd01 = SStmtCall "print"  (SExprBinOp Add (SExprInt 8) (SExprInt 34))

testAdd02 :: SStmt
testAdd02 = SStmtCall "print"   (SExprBinOp Add (SExprBinOp Add (SExprInt 1) (SExprInt 2)) (SExprBinOp Add (SExprInt 3) (SExprInt 4)))

testSub01 :: SStmt
testSub01 = SStmtCall "print"   (SExprBinOp Sub (SExprInt 8) (SExprInt 34))

testSub02 :: SStmt
testSub02 = SStmtCall "print"  (SExprBinOp Add (SExprBinOp Sub (SExprInt 1) (SExprInt 2)) (SExprBinOp Sub (SExprInt 3) (SExprInt 4)))
