module Compiler.CompilerSpec (spec) where

import Test.Hspec
import Compiler.Syntax.SpecLangSrc (specLangSrc)
import Compiler.Syntax.SpecEval(specEval)
import Compiler.Phases.SpecOptArith(specOptArith)

-- Main module for test driver
spec :: Spec
spec = do
  -- Test for Syntax
  specLangSrc
  specEval
  -- Test for Phases
  specOptArith
