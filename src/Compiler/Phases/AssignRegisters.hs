module Compiler.Phases.AssignRegisters where 

import Compiler.Syntax 

import Data.Set (Set)
import qualified Data.Set as Set

-- | uncoverLive: Performs liveness analysis.
-- Discovers which variables are in use in different regions of a program. 
-- A variable or register is live at a program point if its current value 
-- is used at some later point in the program.
uncoverLive :: [InstrVar] -> [(InstrVar, Set AsmVOp)] 
uncoverLive insts = zip insts $ scanr lives Set.empty $ reverse insts
  where
    lives :: InstrVar -> Set AsmVOp -> Set AsmVOp
    lives inst lafter = 
        --  L before (k) = (L after (k) − W(k)) ∪ R(k),
        (lafter `Set.difference` wOps inst) `Set.union` rOps inst

-- | Extract the read operands from an instruction 
-- The callq instruction should include the appropriate 
--   argument-passing registers in its read set R, 
--   depending on the arity of the function being called.
rOps :: InstrVar -> Set AsmVOp
rOps (Instr2 _op s _) = if isImm s then Set.empty else Set.singleton s 
rOps (Instr1 _op sd)  = Set.singleton sd
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
wOps (Instr0 (Callq _s _ar)) = callqWriteSet
wOps (Instr0 _) = Set.empty
wOps (InstrGlob _) = Set.empty
wOps (InstrLabl _) = Set.empty

-- writeSet with all caller saved sets
callqWriteSet :: Set AsmVOp
callqWriteSet = Set.fromList $ VReg <$> calleRSavedRegs

-- | Prepare the edges of the interference graph.
-- For each instruction, create an edge between the locations being written to 
--   and the live locations.
createEdgePairs ::  (InstrVar, [AsmVOp]) -> [(AsmVOp, AsmVOp)]
-- If instruction I k is a move instruction of the form movq s, d, then for every
-- v ∈ L after (k), if v ̸ = d and v ̸ = s, add the edge (d, v).
createEdgePairs ( (Instr2 Movq s d), vs) = [(d,v) | v <- vs, v /= d, v /= s, s /= d]
-- For any other instruction I k , for every d ∈ W(k) and every v ∈ L after (k), 
-- if v ̸ = d, add the edge (d, v).(where W(k) are the locations written to)
createEdgePairs ( (Instr2 _ _ d), vs) = ep d vs
createEdgePairs ( (Instr1 _ d), vs)   = ep d vs
-- For the callq instruction an edge is added between every live variable and
-- every caller-saved register
createEdgePairs ( (Instr0 (Callq _ n)), vs) = [(d,v) | d <- take n argumentPassingRegs, v <- vs]
createEdgePairs ( (Instr0 _          ),  _) = []
createEdgePairs ( (InstrGlob _)       , _ ) = [] 
createEdgePairs ( (InstrLabl _)       , _ ) = [] 

-- Helper Function for createEdgPairs
ep :: AsmVOp -> [AsmVOp] -> [(AsmVOp, AsmVOp)] 
ep d vs = [(d,v) | v <- vs, v /= d ]
