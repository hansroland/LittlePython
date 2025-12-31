{-# LANGUAGE OverloadedRecordDot #-}

module Compiler.Run.Components where 

import Compiler.Run.Options

import Compiler.Syntax
import Compiler.Phases
import Utils

import System.Exit (exitFailure)
import Control.Monad (when, unless)
import Control.Exception
import System.FilePath
import System.Directory
import System.IO
import Control.DeepSeq

-- Run whole compiler 
run :: Settings -> FilePath -> IO () 
run settings srcPath = do 
    ast <- readAndParseSrc settings srcPath
    asm <- compile settings ast 
    postProcessing settings srcPath asm




-- Read and parse input file
readAndParseSrc :: Settings -> FilePath -> IO SProg 
readAndParseSrc settings srcPath = do 
    -- Check existence of input file
    _ <- checkSrcPath srcPath
    -- Read in source input
    src <- readSrcFile srcPath 
    -- Print source input
    when settings.printInp $ 
        printPhaseRslt ("Input: " <> srcPath) src 
    -- Parse
    let parseRslt = parseLpy srcPath src
    ast <- case parseRslt of
        Left err -> do 
            putStrLn err 
            exitFailure
        Right sprog -> do
            return sprog 
    when settings.printAst $ 
        printPhaseRslt "Ast : " $ pp ast
    return ast 

-- the compiler step
compile :: Settings -> SProg -> IO String
compile settings ast = do
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
  let asm = pp $ proEpilog progInstrI
  when settings.printEpilog $ printPhaseRslt "Final Programme" $ asm

  return asm

-- Helper functions

-- Print out the results of a compilation phase
printPhaseRslt :: String -> String -> IO ()
printPhaseRslt title part = do 
  putStrLn title
  putStrLn ""
  putStrLn $ part <> "\n\n"

-- exception safe variant of readFile example: (from deepseq package)
readSrcFile :: FilePath -> IO String
readSrcFile srcPath = bracket (openFile srcPath ReadMode) hClose $ \h ->
                       evaluate . force =<< hGetContents h

checkSrcPath :: FilePath -> IO ()
checkSrcPath srcPath = do 
  exists <- doesFileExist srcPath 
  case exists of 
    True -> pure () 
    False -> do 
      putStrLn $ "ERROR Inputfile " <> srcPath <> " not found!\n"
      dumpUsage options
      exitFailure

postProcessing :: Settings -> FilePath -> String -> IO ()
postProcessing settings srcPath asm = do 
    --Is a runtime in the bin dir
    _ <- checkRuntime
    -- Copy and link
    outname <- copyAsm settings srcPath asm
    _ <- linkAsm outname
    pure ()

-- Check, whether we have to rebuild the runtime 
checkRuntime :: IO ()
checkRuntime = do 
  exist <- doesFileExist "bin/runtime.o"
  unless exist $ do
    res <- gccRuntime
    putStrLn $ "Result gccRuntime" <> res

-- Copy the resulting assembler module to the bin directory
copyAsm :: Settings -> FilePath -> String -> IO (FilePath)
copyAsm settings srcPath asm = do
  let fileNm = takeFileName srcPath 
  let outNm = replaceExtension fileNm ".s"
  let outPath = settings.outdir </> outNm
  writeFile outPath asm
  pure outPath 
