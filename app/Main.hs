{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Compiler.Syntax
import Compiler.Phases
import Options

import System.Exit (exitFailure)
import Control.Monad (when)
import System.FilePath (replaceExtension)
import System.Directory
import System.IO

main :: IO ()
main = do
  settings <- getOptions
  let filename = settings.file 

  fileHandle <- openFile filename ReadMode
  prog <- getFileContents fileHandle
  compile settings filename prog
  hClose fileHandle

-- read file data
getFileContents :: Handle -> IO (String)
getFileContents fileHandle = do 
  fileContent <- hGetContents fileHandle
  pure fileContent

checkFileName :: FilePath -> IO ()
checkFileName filename = do 
  exists <- doesFileExist filename 
  case exists of 
    True -> pure () 
    False -> do 
      dumpUsage options
      exitFailure


-- runthe compiler
compile :: Settings -> String -> String -> IO ()
compile settings filename prog = do 

  -- Input
  when settings.printInp $ prtPart ("Input: " <> filename) prog 
  
  -- Parsing
  let parsResult = parseLpy filename prog
  ast <- case parsResult of 
    Left err -> do 
      print err 
      exitFailure
    Right sprog -> do
      return sprog 
  when settings.printAst $ prtPart "Ast : " $ pp ast

  -- Remove complex instructions
  let progRco = rco ast 
  when settings.printRco $ prtPart "Remove complex operations" $ pp progRco 

  -- Select instructions
  let progInstrV = selectInstr progRco 
  when settings.printSi $ prtPart "Select Instructions" $ pp progInstrV 

  -- Assign Homes
  let progAssignHomes = assignHomes progInstrV
  when settings.printAh $ prtPart "Assign Homes" $ pp progAssignHomes

  -- Patch Instructions
  let progInstrI = patchInstr progAssignHomes
  when settings.printPatch $ prtPart "Patch Instructions" $ pp progInstrI 

  -- Add Prolog and Epilog
  let ppresult = pp $ proEpilog progInstrI
  when settings.printEpilog $ prtPart "Final Programme" $ ppresult

  let outname = replaceExtension filename ".s"
  writeFile outname ppresult 

prtPart :: String -> String -> IO ()
prtPart title part = do 
  putStrLn title
  putStrLn ""
  putStrLn $ part <> "\n\n"
