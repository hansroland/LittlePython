module Compiler.Phases.SelectInstr where

import Compiler.Syntax

selectInstr :: MProg -> ProgAsmV
selectInstr (MProg stmts) = ProgAsmV 0 $ concat $ selectInstrStmt <$> stmts

selectInstrStmt :: MStmt -> [InstrVar]
selectInstrStmt (MStmtCall fun (MAtomVar v)) = 
    [Instr2 Movq (VVar v) (VReg Rdi), Instr0 (Callq fun)]
selectInstrStmt (MStmtCall fun (MAtomInt n)) = 
    [Instr2 Movq (VImm n) (VReg Rdi), Instr0 (Callq fun)]

selectInstrStmt (MStmtAssign v (MExprAtom a )) = [Instr2 Movq (fromAtom a) (VVar v)]
selectInstrStmt (MStmtAssign v (MExprBinOp op atom1 atom2)) = 
    binop op (fromAtom atom1) (fromAtom atom2) (VVar v) 
selectInstrStmt (MStmtAssign v (MExprUOp uop a)) = umop uop (fromAtom a) (VVar v)
selectInstrStmt mstmt = error $ "selectInstr unknown stmt: " <> show mstmt

binop :: BinOp -> AsmVOp -> AsmVOp -> AsmVOp -> [InstrVar]
binop Add op1 op2 res 
    | op2 == res = [Instr2 Addq op1 res]
    | op1 == res = [Instr2 Addq op2 res]
    | otherwise =  [Instr2 Movq op1 (VReg Rax), Instr2 Addq op2 (VReg Rax), Instr2 Movq (VReg Rax) res]      -- var = op1 + var
binop Sub op1 op2 res 
    | otherwise =  [Instr2 Movq op1 (VReg Rax), Instr2 Subq op2 (VReg Rax), Instr2 Movq (VReg Rax) res] 

umop:: UnaryOp -> AsmVOp -> AsmVOp -> [InstrVar]
umop USub op r = [Instr2 Movq op r, Instr1 Negq r]

fromAtom :: MAtom -> AsmVOp
fromAtom (MAtomInt n) = VImm n 
fromAtom (MAtomVar v) = VVar v
