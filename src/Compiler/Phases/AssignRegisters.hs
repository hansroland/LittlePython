{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Compiler.Phases.AssignRegisters where 

import Compiler.Syntax 

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List( nub )

-- | uncoverLive: Performs liveness analysis.
-- Discovers which variables are in use in different regions of a program. 
-- A variable or register is live at a program point if its current value 
-- is used at some later point in the program.
-- `init` we are reversed but want the after set
uncoverLive :: [InstrVar] -> [(InstrVar, Set AsmVOp)] 
uncoverLive insts = zip insts $ reverse $ init $ scanl step Set.empty $ reverse insts 
  where
    step :: Set AsmVOp -> InstrVar -> Set AsmVOp
    step lafter inst = 
        --  L before (k) = (L after (k) − W(k)) ∪ R(k),
        (lafter `Set.difference` wOps inst) `Set.union` rOps inst

-- | Extract the read operands from an instruction 
-- The callq instruction should include the appropriate 
--   argument-passing registers in its read set R, 
--   depending on the arity of the function being called.
rOps :: InstrVar -> Set AsmVOp
-- Movq doesn't read the second operand
rOps (Instr2 Movq s _) = if isImm s then Set.empty else Set.singleton s
rOps (Instr2 _op s d)  = if isImm s then Set.singleton d else Set.fromList [s,d] 
rOps (Instr1 _op sd)   = Set.singleton sd
rOps (Instr0 (Callq _s ar)) = Set.fromList $ take ar argumentPassingRegs
rOps (Instr0 _op) = Set.empty
rOps (InstrGlob _lbl) = Set.empty
rOps (InstrLabl _lbl) = Set.empty

-- | Extract the write operands from an instruction
-- The callq instruction should include all the caller-saved 
--    registers in its write set W because the calling convention says that
--    those registers may be written to during the function call.
wOps :: InstrVar -> Set AsmVOp
wOps (Instr2 _ _ d) = Set.singleton d 
wOps (Instr1 _ sd)  = Set.singleton sd
wOps (Instr0 (Callq _s _ar)) = Set.fromList $ VReg <$> calleRSavedRegs
wOps (Instr0 _) = Set.empty
wOps (InstrGlob _) = Set.empty
wOps (InstrLabl _) = Set.empty

-- | Prepare the edges for the interference graph.
-- For each instruction, create an edge between the locations being 
-- written to and the live locations. (However, a location never 
-- interferes with itself.)
edgePairs :: [InstrVar] -> [(AsmVOp, AsmVOp)]
edgePairs instrs = nub $ concat $ edgePairsByInstr <$> uncoverLive instrs
  where
    edgePairsByInstr :: (InstrVar, Set AsmVOp) -> [(AsmVOp, AsmVOp)]
    edgePairsByInstr (instr@(Instr2 Movq s _), lafter) = 
        [(d,v) | d <- Set.toList $ wOps instr, v <- Set.toList lafter, 
                 d /= v, v /= s, s /= d ] 
    edgePairsByInstr (instr, lafter) = 
      [(d,v) | d <- Set.toList $ wOps instr, v <- Set.toList lafter,  d /= v] 
