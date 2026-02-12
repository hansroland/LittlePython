module Compiler.Phases.AssignHomes (assignHomes) where 

import Compiler.Syntax

import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)

-- | A state monad with a map to store 
--      - a variable -> address dictionary
--      - the current maximal offset from the base pointer
type AssignMonad a = State (Map AsmVOp AsmIOp, Offset) a

-- | Replace variables with addresses on the stack
--     vrmap     : Map to replace variables by registers
--     ProgAsmV  : Assembler program with variables
--     ProgAsmI  : Assembler program with memory addresses
assignHomes :: Int -> Map AsmVOp AsmIOp -> ProgAsmV -> ProgAsmI
assignHomes nCalleeSavRegs vrmap vstmts =
  let 
    -- Calculate size of usedCalleeSave area. Start home addresses for spill variables after.
    szRegSave = 8 * nCalleeSavRegs
    (astmts, (varmap, maxOffset)) = runState 
        (sequence (asHInstr <$> vstmts)) (vrmap, Offset (negate szRegSave))    
 
    -- Assign Homes for Instructions
    -- Translate instructions from Variables to Addresses / Registers
    asHInstr :: InstrVar -> AssignMonad InstrInt 
    asHInstr (Instr2 opc op1 op2) = Instr2 opc <$> (asROp op1) <*> (asROp op2)
    asHInstr (Instr1 opc op1)     = Instr1 opc <$> asROp op1
    asHInstr (Instr0 opc)         = pure $ Instr0 opc  
    asHInstr (InstrCall fn (Just v) atms)  = InstrCall fn <$> (Just <$> (asROp v))  <*> (mapM asROp atms)
    asHInstr (InstrCall fn  Nothing atms)  = InstrCall fn Nothing <$> (mapM asROp atms)
    asHInstr (InstrGlob lbl)      = pure $ InstrGlob lbl
    asHInstr (InstrLabl lbl)      = pure $ InstrLabl lbl
    -- If possible, replace variable by register / home address
    asROp :: AsmVOp -> AssignMonad AsmIOp 
    asROp oprnd = do
        repl <- asHOp oprnd
        pure $ Map.findWithDefault repl oprnd varmap 
    -- Assign Homes for Operands
    asHOp  :: AsmVOp -> AssignMonad AsmIOp
    asHOp (VImm n)   = pure $ IImm n
    asHOp vvar = do 
        (mbvdict, offset) <- get
        case Map.lookup vvar mbvdict of 
            Nothing -> do
                let newOffset = offset - 8 
                let newEntry = IMem newOffset Rbp
                let newDict = Map.insert vvar newEntry mbvdict
                _ <- put (newDict, newOffset)
                return newEntry
            Just op -> return op
  in 
    -- the offset is negative, the size must be positive !!
    ProgAsmI (abs maxOffset)  astmts 
