{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Compiler.Syntax
import Compiler.Phases
import Utils
import Options

import System.Exit (exitFailure)
import Control.Monad (when, unless)
import System.FilePath
import System.Directory
import System.IO

main :: IO ()
main = do
  settings <- getOptions
  let pathname = settings.file 
  checkFileName pathname

  fileHandle <- openFile pathname ReadMode
  prog <- getFileContents fileHandle
  compile settings pathname prog
  hClose fileHandle

-- read file data
getFileContents :: Handle -> IO (String)
getFileContents fileHandle = do 
  fileContent <- hGetContents fileHandle
  pure fileContent

checkFileName :: FilePath -> IO ()
checkFileName pathname = do 
  exists <- doesFileExist pathname 
  case exists of 
    True -> pure () 
    False -> do 
      putStrLn $ "ERROR Inputfile " <> pathname <> " not found!\n"
      dumpUsage options
      exitFailure

-- run the compiler
compile :: Settings -> FilePath -> String -> IO ()
compile settings pathname prog = do 

  -- Input
  when settings.printInp $ printPhaseRslt ("Input: " <> pathname) prog 
  
  -- Parsing
  let parsResult = parseLpy pathname prog
  ast <- case parsResult of 
    Left err -> do 
      print err 
      exitFailure
    Right sprog -> do
      return sprog 
  when settings.printAst $ printPhaseRslt "Ast : " $ pp ast

  -- Remove complex instructions
  let progRco = rco ast 
  when settings.printRco $ printPhaseRslt "Remove complex operations" $ pp progRco 

  -- Select instructions
  let progInstrV = selectInstr progRco 
  when settings.printSi $ printPhaseRslt "Select Instructions" $ pp progInstrV 

  -- Assign Homes
  let progAssignHomes = assignHomes progInstrV
  when settings.printAh $ printPhaseRslt "Assign Homes" $ pp progAssignHomes

  -- Patch Instructions
  let progInstrI = patchInstr progAssignHomes
  when settings.printPatch $ printPhaseRslt "Patch Instructions" $ pp progInstrI 

  -- Add Prolog and Epilog
  let ppresult = pp $ proEpilog progInstrI
  when settings.printEpilog $ printPhaseRslt "Final Programme" $ ppresult
  pure ()

-- TODO Move this to an own function
  -- Check on Runtime
  checkRuntime

  -- Copy and link
  outname <- copyAsm pathname ppresult settings
  _ <- linkAsm outname
  
  pure ()


-- Copy the resulting assembler module to the bin directory
copyAsm :: FilePath -> String -> Settings -> IO (FilePath)
copyAsm pathNm asm settings = do
  let fileNm = takeFileName pathNm 
  let outNm = replaceExtension fileNm ".s"
  let outPath = settings.outdir </> outNm
  writeFile outPath asm
  pure outPath 

-- Check, whether we have to rebuild the runtime 
checkRuntime :: IO ()
checkRuntime = do 
  exist <- doesFileExist "bin/runtime.o"
  unless exist $ do
    res <- gccRuntime
    putStrLn $ "Result gccRuntime" <> res

-- Print out the results of a compilation phase
printPhaseRslt :: String -> String -> IO ()
printPhaseRslt title part = do 
  putStrLn title
  putStrLn ""
  putStrLn $ part <> "\n\n"
