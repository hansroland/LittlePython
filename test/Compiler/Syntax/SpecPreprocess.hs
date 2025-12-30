{-# LANGUAGE OverloadedStrings #-}

module Compiler.Syntax.SpecPreprocess (specPreprocess) where 

import Utils
import Test.Hspec
import System.FilePath
import System.Directory(copyFile)

-- Copy the lpy compiler to the bin directory of the project.
copyCompiler :: IO ()
copyCompiler = do
    src <- getExePath
    let fn = takeFileName src 
    let dest = "./bin" </> fn 
    copyFile src dest

-- Dummy test, just to copy the exec built by 'cabal build' to the bin directory.
specPreprocess :: Spec
specPreprocess = (before_ (copyCompiler)) $ do 
        describe "Phantom Test: Copy lpy exec to bin directory" $ do
            it "Preprocess - copyCompiler" $ do True `shouldBe` True
