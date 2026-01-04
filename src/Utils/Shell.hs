{-# LANGUAGE OverloadedStrings #-}

module Utils.Shell where 

import System.Process hiding (runProcess)
import System.Exit
import System.FilePath
import System.IO

-- Get the path where cabal hides the executable
-- testProcess :: IO FilePath 
-- testProcess = do 
--     (_, Just hout, _, _) <-  createProcess (proc "ls" []){ std_out = CreatePipe }
--    let params = CreateProcess {
--        std_out = CreatePipe
--    }

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

compileAndRun :: FilePath -> IO String 
compileAndRun prog = do 
    _ <- runProcess $ "bin" </> "lpy examples" </> prog <.> "lpy"
    runProcess $ "bin" </> prog
