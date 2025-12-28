module Main where

import Compiler.Syntax
import Compiler.Phases

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO

-- See: https://stackoverflow.com/questions/47683804/parsing-command-line-arguments-in-haskell
--      https://stackoverflow.com/questions/28927358/how-to-get-leftover-arguments-in-optparse-applicative

main :: IO ()
main = do
  filename <- getFilePath 
  fileHandle <- openFile filename ReadMode
  prog <- getFileContents fileHandle
  compile filename prog
  hClose fileHandle

     -- get filepath to compile
getFilePath :: IO String
getFilePath = do
  args <- getArgs
  case length args of
      1 -> do
         pure $ head args
      _ -> do
         putStrLn "usage:\r lpy <filePath>\r <filePath> = *.lpy file to compile"
         exitFailure

-- read file data
getFileContents :: Handle -> IO (String)
getFileContents fileHandle = do 
  fileContent <- hGetContents fileHandle
  pure fileContent

-- runthe compiler
compile :: String -> String -> IO ()
compile filename prog = do 

  prtPart "Input" prog 

  let parsResult = parseLpy filename prog
  ast <- case parsResult of 
    Left err -> do 
      print err 
      exitFailure
    Right sprog -> do
      return sprog 

  prtPart "Ast : " $ pp ast

  let progRco = rco ast 
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

prtPart :: String -> String -> IO ()
prtPart title part = do 
  putStrLn title
  putStrLn ""
  putStrLn $ part <> "\n\n\n"

