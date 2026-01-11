module Compiler.Syntax.LangXAsm86 where 

-- This module describes the used portion of the X86-64 assembler. 
-- Asm86Var and Asm86Int are in the same module, but are different types.
-- We use the syntax of the x86 AT&T syntax assembly

-- I don't have a separate type for source and destination operands
-- This would give a lot of nearly identical code.
import Compiler.Syntax.LangBase
import Data.Char(toLower)
import Data.List (intercalate)

-- Registers
data Reg = Rsp | Rbp | Rax | Rbx | Rcx | Rdx | Rsi | Rdi |
        R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
        deriving (Show, Eq, Ord)

-- Operand type for X86Int
data AsmIOp = IReg !Reg                       
           | IMem !Int !Reg 
           | IImm !Int           
           deriving (Show, Eq)

-- Operand type for X86Var
data AsmVOp = VReg !Reg
           | VVar !String
           | VImm !Int
           deriving (Show, Eq, Ord)

data AsmOpc2 = Addq 
            | Subq
            | Movq
           deriving (Show, Eq)

data AsmOpc1 = Negq 
            | Pushq 
            | Popq
           deriving (Show, Eq)

data AsmOpc0 = Callq String Int  -- functionNm arity            
             | Retq
           deriving (Show, Eq)

-- Polymorphic instruction type
data Instr o = Instr2 AsmOpc2 !o !o 
             | Instr1 AsmOpc1 !o 
             | Instr0 AsmOpc0
             | InstrGlob String 
             | InstrLabl String
           deriving (Show, Eq)

-- Instructions for the X86Int language
type InstrInt = Instr AsmIOp

-- Instructions for the X86Var language
type InstrVar = Instr AsmVOp

-- A programs for the ProgAsmVar language
data ProgAsmV = ProgAsmV Int [InstrVar]   

-- A programs for the ProgAsmInt language
data ProgAsmI = ProgAsmI Int [InstrInt]         -- In for frame size in bytes

-- Instances
instance PP Reg where 
    pp r = '%' : (toLower <$> (show r))

instance PP AsmIOp where 
    pp (IReg r)   = pp r
    pp (IMem n r) = concat [show n, "(", pp r,  ")"]
    pp (IImm n)   = '$' : show n

instance PP AsmVOp where 
    pp (VReg r)   = pp r
    pp (VVar str) = str
    pp (VImm n)   = '$' : show n

instance PP AsmOpc2 where 
    pp op = toLower <$> show op
    
instance PP AsmOpc1 where 
    pp op = toLower <$> show op

instance PP AsmOpc0 where 
    pp op = toLower <$> show op

instance (PP top) => PP (Instr top)  where
    pp (Instr2 op s d) = concat [leftm, pp op, "  ", pp s, ", ", pp d]
    pp (Instr1 op sd)  = concat [leftm, pp op, "  ",  pp sd]
    pp (Instr0 (Callq s ar)) = concat [leftm, "callq ", s, " # arity:", show ar]
    pp (Instr0 op) = concat [leftm, pp op]
    pp (InstrGlob lbl) = concat [leftm, ".globl ", lbl]
    pp (InstrLabl lbl) = concat [lbl, ":"]
    
instance PP ProgAsmV where 
    pp (ProgAsmV _ ii) = pp ii

instance PP ProgAsmI where 
    pp (ProgAsmI _ ii) = concat $ [pp ii, "\n"]     -- final newline !!

-- Print lists of instructions separated with newline
instance (PP top) => PP [Instr top] where 
    pp instrs = intercalate "\n" (pp <$> instrs)

-- Helper functions
leftm :: String
leftm = replicate 4 ' '

-- | Check, whether an operand is an immediate operand
isImm :: AsmVOp -> Bool
isImm  (VImm _) = True 
isImm  _        = False


-- | CalleR saved registers
calleRSavedRegs :: [Reg]
calleRSavedRegs = [Rax, Rcx, Rdx, Rdi, Rsi, R8, R9, R10, R11]

calleESavedRegs :: [Reg]
calleESavedRegs = [Rsp, Rbp, Rbx, R12, R13, R14, R15] 

argumentPassingRegs :: [AsmVOp]
argumentPassingRegs = VReg <$> [Rdi, Rsi, Rdx, Rcx, R8, R9]



