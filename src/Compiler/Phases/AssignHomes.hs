module Compiler.Phases.AssignHomes (assignHomes) where 

import Compiler.Syntax

import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)

emptydict :: Map String AsmIOp
emptydict = Map.empty

-- | A state monad with a map to store a variable -> address dictionary
type AssignMonad a = State (Map String AsmIOp) a

-- | Replace variables with addresses on the stack
--     ProgAsmV : Assembler with variables
--     ProgAsmI . Assembler without variables
assignHomes :: ProgAsmV -> ProgAsmI 
assignHomes (ProgAsmV _ vstmts) =
   let 
       (astmts, vardict) = runState (sequence (asHInstr <$> vstmts)) emptydict
       frameSize = Map.size vardict
   in ProgAsmI frameSize $ astmts 

-- Assign Homes for Instructions
asHInstr :: InstrVar -> AssignMonad InstrInt 
asHInstr (Instr2 opc op1 op2) = Instr2 opc <$> (asHOp op1) <*> (asHOp op2)
asHInstr (Instr1 opc op1)     = Instr1 opc <$> asHOp op1
asHInstr (Instr0 opc)         = pure $ Instr0 opc  
asHInstr (InstrGlob lbl)      = pure $ InstrGlob lbl
asHInstr (InstrLabl lbl)      = pure $ InstrLabl lbl

-- Assign Homes for Operands
asHOp  :: AsmVOp -> AssignMonad AsmIOp
asHOp (VReg r)   = pure $ IReg r 
asHOp (VImm n)   = pure $ IImm n
asHOp (VVar vnam) = do 
    mbvdict <- get
    let mbmem = Map.lookup vnam mbvdict 
    case mbmem of 
        Nothing -> do
            let offset = -8 * (1 + Map.size mbvdict) 
            let newEntry = IMem offset Rbp
            let newdict = Map.insert vnam newEntry mbvdict
            _ <- put newdict
            return newEntry
        Just op -> return op
