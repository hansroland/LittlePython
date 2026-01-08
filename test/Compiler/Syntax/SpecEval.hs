module Compiler.Syntax.SpecEval (specEval) where

import Test.Hspec
import Compiler.Run
import Utils
import Compiler.Syntax
import System.FilePath
import System.IO.Silently

exampleDir :: FilePath 
exampleDir = "examples"

-- Evaluate / interpret a SrcLang Program with input.
-- Use redirect for input and output
evalProgWith :: FilePath -> FilePath -> IO String  --includes get_int
evalProgWith prog infn = do 
    sprog <- readAndParseSrc (testSettings $ exampleDir </> prog <.> "lpy")
    out <- runWithInput (evalSProg sprog) (exampleDir </> infn)
    pure out

-- Evaluate / interpret a statement in the IO Monad and capture output
--   The statements do not contain any input operations eg getInt !
evalStmt :: SStmt -> IO String
evalStmt e = do
    output <- capture_ $ evalSProg $ SProg [e]
    pure output


-- evalStmtWith :: SStmt -> IO String               -- without get_int
-- evalStmtWith stmt = do
--     runWithInput (evalSProg (SProg [stmt])) (exampleDir </> "empty")

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
        evalProgWith "prog01" "empty" `shouldReturn` "32\n"

    it "evalProgWith prog02" $ do
        evalProgWith "prog02" "empty" `shouldReturn` "42\n"

    it "evalProgWith prog03" $ do
        evalProgWith "prog03" "empty" `shouldReturn` "-20\n"

    it "evalProgWith prog04" $ do
        evalProgWith "prog04" "empty" `shouldReturn` "-25\n"
    it "compileAndRun prog04" $ do
        compileAndRun "prog04" "input01.txt" `shouldReturn` "-25\n"

    it "evalProgWith (with input) prog05" $ do 
        evalProgWith "prog05" "input01.txt" `shouldReturn` "110\n" 
    it "compileAndRun prog05" $ do
        compileAndRun "prog05" "input01.txt" `shouldReturn` "110\n"

    it "compileAndRun book4_1" $ do
        compileAndRun "book4_1" "input01.txt" `shouldReturn` "42\n"

    it "compileAndRun book4_2" $ do
        compileAndRun "book4_2" "input01.txt" `shouldReturn` "152\n"


-- TODO  - multiple print statements don't work yet !! !!   
--    it "compileAndRun prog06" $ do 
--        compileAndRun "prog06"  "input.txt" `shouldReturn` "32\n45\n"

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
