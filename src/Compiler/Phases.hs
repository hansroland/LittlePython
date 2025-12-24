module Compiler.Phases (
    optimizeSProg
    , optimizeSExpr
    , rco
    , selectInstr
    , assignHomes
    , patchInstr
    , proEpilog
    )
where

import Compiler.Phases.OptSExpr
import Compiler.Phases.Rco
import Compiler.Phases.SelectInstr
import Compiler.Phases.AssignHomes
import Compiler.Phases.PatchInstr
import Compiler.Phases.ProEpilog
