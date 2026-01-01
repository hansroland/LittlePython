{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE CPP #-}

module Main where

import Compiler.Run

main :: IO ()
main = do
  -- Print the createion date of the compiler 
  putStrLn $ concat ["lpy version: ",  __DATE__, " ", __TIME__]
  -- Get the settings
  settings <- getOptions
  -- Run the compiler
  run settings
