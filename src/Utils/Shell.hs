{-# LANGUAGE OverloadedStrings #-}

module Utils.Shell where 

import System.Process hiding (runProcess)
import System.Exit
import System.FilePath
import System.IO

-- Attention: For big outputs this may result in a deadlock
--     https://passingcuriosity.com/2015/haskell-reading-process-safe-deadlock/

runProcess  :: String -> IO String
runProcess cmd = do
    let params = (shell cmd)
            { std_in  = Inherit
            , std_out = CreatePipe
            , std_err = Inherit
            }
    (Nothing, Just out, Nothing, ph) <- createProcess params
    ec <- waitForProcess ph
    case ec of
        ExitSuccess   -> hGetContents out
        ExitFailure _ -> error "Error in Shell.hs runProcess"

-- Right trim newline
-- Remove the newlines at the end of a string
rtrimnl :: String -> String
rtrimnl = reverse . dropWhile (=='\n') . reverse 

-- The program name is without directory and extension 
-- The inputfile name is without directory
compileAndRun :: FilePath -> FilePath -> IO String 
compileAndRun prog inp = do 
    -- compile the lpy program
    let compilr = "bin" </> "lpy"
    let src = "examples" </> prog <.> "lpy"
    _ <- runProcess $ concat [compilr, " ", src]
    -- Run the compiled program
    let exe = "bin" </> prog
    let inpath = "examples" </> inp
    let runCmd = concat [exe, " <", inpath]
    runProcess runCmd
