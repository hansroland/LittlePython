-- See: https://discourse.haskell.org/t/how-to-capture-io/8128/8

module Utils.Redir where

import Control.Exception (bracket, finally)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO (stdin, stdout, withFile, IOMode(ReadMode, WriteMode), hClose)
import System.IO.Temp (writeSystemTempFile, emptySystemTempFile)

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

runWithInput :: IO a -> String -> IO String
runWithInput action input = do
  inputFileName <- writeSystemTempFile "input.txt" input
  outputFileName <- emptySystemTempFile "output.txt"
  _ <- redirect action inputFileName outputFileName
  readFile outputFileName
