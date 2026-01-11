module Compiler.Phases (
    parseLpy
    , optimizeSProg
    , optimizeSExpr
    , rco
    , selectInstr
    , uncoverLive 
    , createEdgePairs
    , assignHomes
    , patchInstr
    , proEpilog
    )
where

import Compiler.Phases.Parser
import Compiler.Phases.OptSExpr
import Compiler.Phases.Rco
import Compiler.Phases.SelectInstr
import Compiler.Phases.AssignRegisters
import Compiler.Phases.AssignHomes
import Compiler.Phases.PatchInstr
import Compiler.Phases.ProEpilog
