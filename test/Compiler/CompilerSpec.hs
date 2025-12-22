module Compiler.CompilerSpec (spec) where

import Test.Hspec
import Compiler.Syntax.SpecLangSrc (specLangSrc)
import Compiler.Syntax.SpecLangSrcMon (specLangSrcMon)
import Compiler.Syntax.SpecLangXAsm86 (specLangXAsm86)
import Compiler.Syntax.SpecEval(specEval)
import Compiler.Phases.SpecOptSExpr(specOptSExpr)
import Compiler.Phases.SpecRco(specRco)
import Compiler.Phases.SpecSelectInstr(specSelInstr)
import Compiler.Phases.SpecAssignHomes(specAssignHomes)

-- Main module for test driver
spec :: Spec
spec = do
  -- Test for Syntax
  specLangSrc
  specEval
  specLangSrcMon
  specLangXAsm86
  -- Test for Phases
  specOptSExpr
  specRco
  specSelInstr 
  specAssignHomes
