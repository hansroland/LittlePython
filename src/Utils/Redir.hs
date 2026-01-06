-- See: https://discourse.haskell.org/t/how-to-capture-io/8128/8

module Utils.Redir where

import Control.Exception (bracket, finally, evaluate)
import GHC.IO.Handle (hDuplicate, hDuplicateTo, hGetContents)
import System.IO (stdin, stdout, openFile, withFile, IOMode(ReadMode, WriteMode), hClose)
import System.IO.Temp (emptySystemTempFile)
import Control.DeepSeq


redirect :: IO r -> FilePath -> FilePath -> IO r
redirect action inputFileName outputFileName = do
  withFile inputFileName ReadMode $ \hIn ->
    withFile outputFileName WriteMode $ \hOut ->
      bracket
        (liftA2 (,) (hDuplicate stdin) (hDuplicate stdout))
        (\(old_stdin, old_stdout) ->
           (hDuplicateTo old_stdin stdin >> hDuplicateTo old_stdout stdout)
           `finally`
           (hClose old_stdin >> hClose old_stdout))
        (\_ ->
           do
             hDuplicateTo hIn stdin
             hDuplicateTo hOut stdout
             action)

runWithInput :: IO a -> FilePath -> IO String
runWithInput action inputFileName = do
--   inputFileName <- writeSystemTempFile "input.txt" input
  outputFileName <- emptySystemTempFile "output.txt"
  _ <- redirect action inputFileName outputFileName
  readFile outputFileName

-- exception safe variant of readFile example: (from deepseq package)
getFileContents :: FilePath -> IO String
getFileContents srcPath = bracket (openFile srcPath ReadMode) hClose $ \h ->
                       evaluate . force =<< hGetContents h

