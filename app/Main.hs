module Main where

import Compiler.Syntax
import Compiler.Phases


-- https://www.baeldung.com/linux/assembly-compile-run
-- https://www.jdoodle.com/compile-assembler-gcc-online

main :: IO ()
main = do 
  let prog = "prog02"

  prtPart "Source : LangSrc" $ pp prog02

  let progRco = rco prog02 
  prtPart "Remove complex operations" $ pp progRco 

  let progInstrV = selectInstr progRco 
  prtPart "Select Instructions" $ pp progInstrV 

  let progAssignHomes = assignHomes progInstrV
  prtPart "Assign Homes" $ pp progAssignHomes

  let progInstrI = patchInstr progAssignHomes
  prtPart "Patch Instructions" $ pp progInstrI 

  let ppresult = pp $ proEpilog progInstrI
  prtPart "Final Programme" $ ppresult

  writeFile (prog <> ".s") ppresult 

prog02 :: SProg 
prog02 = SProg [
    SStmtAssign "a" (SExprInt 42),
    SStmtAssign "b" (SExprVar "a"), 
    SStmtCall "print_int" (SExprVar "b")]

prtPart :: String -> String -> IO ()
prtPart title part = do 
  putStrLn title
  putStrLn ""
  putStrLn $ part <> "\n\n\n"

