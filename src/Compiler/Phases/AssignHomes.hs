module Compiler.Phases.AssignHomes (assignHomes) where 

import Compiler.Syntax

import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)

-- | A state monad with a map to store a variable -> address dictionary
type AssignMonad a = State (Map AsmVOp AsmIOp) a

-- | Replace variables with addresses on the stack
--     vrmap     : Map to replace variables by registers
--     ProgAsmV  : Assembler with variables
--     ProgAsmI  : Assembler without variables
assignHomes :: Map AsmVOp AsmIOp -> ProgAsmV -> ProgAsmI
assignHomes vrmap vstmts =
  let 
    (astmts, varmap) = runState (sequence (asHInstr <$> vstmts)) vrmap
    frameSize = calcFrameSize varmap      
 
    -- Assign Homes for Instructions
    -- Translate from Variables to Addresses
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
        mbvdict <- get
        case Map.lookup vvar mbvdict of 
            Nothing -> do
                let offset = -8 * (1 + Map.size mbvdict) 
                let newEntry = IMem offset Rbp
                let newdict = Map.insert vvar newEntry mbvdict
                _ <- put newdict
                return newEntry
            Just op -> return op
    -- Calculate the size of the frame
    calcFrameSize :: Map AsmVOp AsmIOp -> Int    
    calcFrameSize varmap = Map.size varmap     -- TODO filter out registers
                                               --      add save registers
  in ProgAsmI frameSize $ astmts 
