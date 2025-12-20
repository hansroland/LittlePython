module Compiler.Syntax.LangXAsm86 where 

-- This module describes the used portion of the X86-64 assembler. 
-- Asm86Var and Asm86Int are in the same module, but different types.

-- I don't have a separate type for source and destination operands
-- This would give a lot of nearly identical code.
import Compiler.Syntax.LangBase
import Data.Char(toLower)


-- Registers
data Reg = Rsp | Rbp | Rax | Rbx | Rcx | Rdx | Rsi | Rdi |
        R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
        deriving (Show, Eq)

-- Operand type for X86Int
data AsmIOp = IReg !Reg                       
           | IMem !Int !Reg 
           | IImm !Int           
           deriving (Show, Eq)

-- Operand type for X86Var
data AsmVOp = VReg !Reg
           | VMem !String
           | VImm !Int
           deriving (Show, Eq)

-- Polymorphic instruction type
data Instr o = Addq !o !o 
           | Subq !o !o 
           | Negq !o 
           | Movq !o !o
           | Pushq !o 
           | Popq !o
           | Callq !String
           | Retq 
           deriving (Show)

-- Instructions for the X86Int language
type InstrInt = Instr AsmIOp

-- Instructions for the X86Var language
type InstrVar = Instr AsmVOp

data ProgAsmV = ProgAsmV [InstrVar]

data ProgAsmI = ProgAsmI [InstrInt]

-- Instances
instance PP Reg where 
    pp r = '%' : (toLower <$> (show r))

instance PP AsmIOp where 
    pp (IReg r)   = pp r
    pp (IMem n r) = concat ["(", show n, ")", pp r ]
    pp (IImm n)   = '$' : show n

instance PP AsmVOp where 
    pp (VReg r)   = pp r
    pp (VMem str) = str
    pp (VImm n)   = '$' : show n

instance (PP top) => PP (Instr top)  where
    pp (Addq s d) = concat [leftm, "addq  ", ppsd s  d]
    pp (Subq s d) = concat [leftm, "subq  ", ppsd s  d]
    pp (Negq s)   = concat [leftm, "negq  ", pp s]
    pp (Movq s d) = concat [leftm, "movq  ", ppsd s  d]
    pp (Pushq d)  = concat [leftm, "pushq ", pp d]
    pp (Popq d)   = concat [leftm, "popq  ", pp d]
    pp (Callq st) = concat [leftm, "callq ", st]
    pp  Retq      = concat [leftm, "retq  "]

-- Helper functions
leftm :: String
leftm = replicate 4 ' '

ppsd :: (PP s, PP d) => s -> d -> String 
ppsd s d = concat [pp s, ", ", pp d]
