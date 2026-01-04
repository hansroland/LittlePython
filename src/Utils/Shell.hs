{-# LANGUAGE OverloadedStrings #-}

module Utils.Shell where 

import Shellmet 
import System.FilePath
import qualified Data.Text as T

-- Get the path where cabal hides the executable
getExePath :: IO FilePath
getExePath =  do   
    expath <- "cabal" $| ["list-bin", "lpy"] 
    pure $ T.unpack expath

linkAsm :: FilePath ->  IO String
linkAsm inPath  = do 
    res <- "gcc" $| [T.pack inPath, "bin/runtime.o", "-o", T.pack (dropExtension inPath)]
    pure $ T.unpack res

gccRuntime :: IO String 
gccRuntime = do
    res <- "gcc" $| ["-c", "src/Cbits/runtime.c", "-o", "bin/runtime.o"]
    pure $ T.unpack res

compileAndRun :: FilePath -> IO String 
compileAndRun prog = do 
    _ <- ("bin" </> "lpy") $| [T.pack ("examples" </> prog <.> ".lpy")]
    res <- ("bin" </> prog) $| [T.empty]
    pure $ (T.unpack res) ++ "\n" 
