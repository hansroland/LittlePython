module Compiler.Phases (
    optimizeSProg
    , optimizeSExpr
    , rco
    , selectInstr
    )
where

import Compiler.Phases.OptArith
import Compiler.Phases.Rco
import Compiler.Phases.SelectInstr

