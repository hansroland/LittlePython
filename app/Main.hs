{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE CPP #-}

module Main where

import Compiler.Run

main :: IO ()
main = do
  -- Get the settings
  settings <- getOptions
  if settings.printVersion 
    then 
      -- Print the creation date of the compiler 
      putStrLn $ concat ["lpy version: ",  __DATE__, " ", __TIME__]
    else
      -- Run the compiler
      run settings
