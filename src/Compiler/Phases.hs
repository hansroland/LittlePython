module Compiler.Phases (
    optimizeSProg
    , optimizeSExpr
    , rco
    , selectInstr
    , assignHomes
    )
where

import Compiler.Phases.OptSExpr
import Compiler.Phases.Rco
import Compiler.Phases.SelectInstr
import Compiler.Phases.AssignHomes

