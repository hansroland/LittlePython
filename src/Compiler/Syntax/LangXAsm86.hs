{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler.Syntax.LangXAsm86 where 

-- This module describes the used portion of the X86-64 assembler. 
-- Asm86Var and Asm86Int are in the same module, but are different types.
-- We use the syntax of the x86 AT&T syntax assembly

-- I don't have a separate type for source and destination operands
-- This would give a lot of nearly identical code.
import Compiler.Syntax.LangBase
import Data.Char(toLower)
import Data.List (intercalate)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map as Map

-- Registers
data Reg = Rsp | Rbp | Rax | Rbx | Rcx | Rdx | Rsi | Rdi |
        R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
        deriving (Show, Eq, Ord)

-- Offsets are displacements in bytes from a base register
newtype Offset = Offset { unoffset :: Int }
    deriving (Eq, Show, Num, Ord, Integral, Real, Enum)
    -- for conversion use: `unoffset`, or type annotation `:: Offset`

instance PP Offset where 
    pp (Offset n) = show n

-- Operand type for X86Int
data AsmIOp = IReg !Reg                       
           | IMem !Offset !Reg 
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

data AsmOpc0 = Retq
           deriving (Show, Eq)

-- Polymorphic instruction type
data Instr o = Instr2 AsmOpc2 !o !o 
             | Instr1 AsmOpc1 !o 
             | Instr0 AsmOpc0
             | InstrCall String !(Maybe o) ![o]  
             | InstrGlob String 
             | InstrLabl String
           deriving (Show, Eq)

-- Instructions for the X86Int language
type InstrInt = Instr AsmIOp

-- Instructions for the X86Var language
type InstrVar = Instr AsmVOp

-- A programs for the ProgAsmVar language is just a list of instructions
type ProgAsmV = [InstrVar]   

-- A programs for the ProgAsmInt language
data ProgAsmI = ProgAsmI Offset [InstrInt]         -- Offset for frame size in bytes

-- Instances
instance PP Reg where 
    pp r = '%' : (toLower <$> (show r))

instance PP AsmIOp where 
    pp (IReg r)   = pp r
    pp (IMem n r) = concat [show (unoffset n), "(", pp r,  ")"]
    pp (IImm n)   = '$' : show n

instance PP AsmVOp where 
    pp (VReg r)   = pp r
    pp (VVar str) = str
    pp (VImm n)   = '$' : show n 

instance PP [(AsmVOp, AsmVOp)] where 
    pp lst = concat ["[", intercalate ", " (pp <$> lst), "]"]

instance PP [(AsmVOp, AsmIOp)] where 
    pp lst = concat ["[", intercalate ", " (pp <$> lst), "]"]

instance PP [AsmVOp] where
    pp oprs = intercalate " " $ pp <$> oprs

instance PP [AsmIOp] where
    pp oprs = intercalate " " $ pp <$> oprs

 
instance PP (Set AsmVOp) where 
    pp set = concat ["{", intercalate " " (pp <$> Set.elems set), "}"]

instance PP [Set AsmVOp] where 
    pp sets = intercalate "\n" (pp <$> sets)

instance PP (Map AsmVOp AsmVOp) where 
    pp mymap = pp $ Map.toList mymap

instance PP [(InstrVar, Set AsmVOp)] where
    pp plist = intercalate "\n" (pp <$> plist)

instance PP AsmOpc2 where 
    pp op = toLower <$> show op
    
instance PP AsmOpc1 where 
    pp op = toLower <$> show op

instance PP AsmOpc0 where 
    pp op = toLower <$> show op

instance PP top => PP (Instr top)  where
    pp (Instr2 op s d) = concat [leftm, pp op, "  ", pp s, ", ", pp d]
    pp (Instr1 op sd)  = concat [leftm, pp op, "  ",  pp sd]
    pp (InstrCall fn Nothing atms) = (concat [leftm, "callq ", fn,  " # args:"])
       <> intercalate " " (pp <$> atms)
    pp (InstrCall fn (Just rslt) atms) = (concat [leftm, pp rslt ," = callq ", fn, " # args:"])
       <> intercalate " " (pp <$> atms)
    pp (Instr0 op) = concat [leftm, pp op]
    pp (InstrGlob lbl) = concat [leftm, ".globl ", lbl]
    pp (InstrLabl lbl) = concat [lbl, ":"]
    
instance PP ProgAsmI where 
    pp (ProgAsmI _ ii) = concat $ [pp ii, "\n"]     -- final newline !!

-- Print lists of instructions separated with newline
instance (PP top) => PP [Instr top] where 
    pp instrs = intercalate "\n" (pp <$> instrs)

-- Helper functions
leftm :: String
leftm = replicate 4 ' '

-- | Check, whether an operand is an immediate operand
isVImm :: AsmVOp -> Bool
isVImm  (VImm _) = True 
isVImm  _        = False

-- | Check, whether an operand is a variable operand
isVVar :: AsmVOp -> Bool 
isVVar (VVar _) = True 
isVVar _        = False 

isVReg :: AsmVOp -> Bool 
isVReg (VReg _ ) = True 
isVReg _         = False

-- | Check, whether aan opearnd is eiter a variable or a register 
isVVarOrVReg :: AsmVOp -> Bool 
isVVarOrVReg (VReg _ ) = True 
isVVarOrVReg (VVar _ ) = True 
isVVarOrVReg (VImm _ ) = False

-- | CalleR saved registers
calleRSavedRegs :: [Reg]
calleRSavedRegs = [Rax, Rcx, Rdx, Rdi, Rsi, R8, R9, R10, R11]

-- | Check, whether an address operand is a memory address
isIMem ::  AsmIOp -> Bool 
isIMem (IMem _ _) = True 
isIMem _          = False

isIReg ::  AsmIOp -> Bool 
isIReg (IReg _) = True 
isIReg _        = False

calleESavedRegs :: [Reg]
calleESavedRegs = [Rsp, Rbp, Rbx, R12, R13, R14, R15] 

argumentPassingRegs :: [AsmIOp]
argumentPassingRegs = IReg <$> [Rdi, Rsi, Rdx, Rcx, R8, R9]
