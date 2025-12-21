module Compiler.Phases.SelectInstr where

import Compiler.Syntax

selectInstr :: MProg -> ProgAsmV
selectInstr (MProg stmts) = ProgAsmV $ concat $ selectInstrStmt <$> stmts

selectInstrStmt :: MStmt -> [InstrVar]
selectInstrStmt (MStmtCall fun  (MExprAtom(MAtomVar v))) = 
    [Movq (VMem v) (VReg Rdi), (Callq fun)]
selectInstrStmt (MStmtCall fun (MExprAtom(MAtomInt n))) = 
    [Movq (VImm n) (VReg Rdi), (Callq fun)]
selectInstrStmt (MStmtCall fun ex) = 
    error $ concat ["selectInstrStmt (MStmtCall fun ex) ", show fun, " ", show ex]

selectInstrStmt (MStmtAssign v (MExprAtom a )) = [Movq (fromAtom a) (VMem v)]
selectInstrStmt (MStmtAssign v (MExprBinOp op atom1 atom2)) = 
    binop op (fromAtom atom1) (fromAtom atom2) (VMem v) 
selectInstrStmt (MStmtAssign v (MExprUOp uop a)) = umop uop (fromAtom a) (VMem v)

-- selectInstrStmt (MStmtExpr (MExprAtom (MAtomInt n)))

selectInstrStmt mstmt = error $ "selectInstr unknown stmt: " <> show mstmt


binop :: BinOp -> AsmVOp -> AsmVOp -> AsmVOp -> [InstrVar]
binop Add op1 op2 res 
    | op2 == res = [Addq op1 res]
    | op1 == res = [Addq op2 res]
    | otherwise =  [Movq op1 (VReg Rax), Addq op2 (VReg Rax), Movq (VReg Rax) res]      -- var = op1 + var
binop Sub op1 op2 res 
    | otherwise =  [Movq op1 (VReg Rax), Subq op2 (VReg Rax), Movq (VReg Rax) res] 

umop:: UnaryOp -> AsmVOp -> AsmVOp -> [InstrVar]
umop USub op r = [Movq op r, Negq r]

fromAtom :: MAtom -> AsmVOp
fromAtom (MAtomInt n) = VImm n 
fromAtom (MAtomVar v) = VMem v
