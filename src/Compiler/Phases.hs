module Compiler.Phases (
    parseLpy
    , optimizeSProg
    , optimizeSExpr
    , rco
    , selectInstr
    , assignRegisters
    , uncoverLive 
    , edgePairs
    , wOps
    , rOps
    , colorGraph
    , assignHomes
    , regs0vars
    , regs4vars
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
