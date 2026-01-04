module Compiler.Phases.SelectInstr where

import Compiler.Syntax

selectInstr :: MProg -> ProgAsmV
selectInstr (MProg stmts) = ProgAsmV 0 $ concat $ siStmt <$> stmts

siStmt :: MStmt -> [InstrVar]
siStmt (MStmtCall fun atom )  = siStmtCall fun atom 
siStmt (MStmtAssign fun expr) = siStmtAssign fun expr
siStmt (MStmtExpr e ) = siStmtExpr e

siStmtCall :: String -> MAtom -> [InstrVar]
siStmtCall fun (MAtomVar v) =
    [Instr2 Movq (VVar v) (VReg Rdi), Instr0 (Callq (fixFuncName fun))]
siStmtCall fun (MAtomInt n) = 
    [Instr2 Movq (VImm n) (VReg Rdi), Instr0 (Callq (fixFuncName fun))]

siStmtAssign :: String -> MExpr -> [InstrVar]
siStmtAssign v (MExprAtom a ) = [Instr2 Movq (fromAtom a) (VVar v)]
siStmtAssign v (MExprBinOp op atom1 atom2) = 
    binop op (fromAtom atom1) (fromAtom atom2) (VVar v) 
siStmtAssign v (MExprUOp uop a) = umop uop (fromAtom a) (VVar v)
siStmtAssign _ e = error $ "selectInstr.siStmtAssign NOT_IMPL e: " <> show e

siStmtExpr :: MExpr -> [InstrVar]
siStmtExpr (MExprFunc var fun) = 
    [Instr0 (Callq (fixFuncName fun)), Instr2 Movq (VReg Rax) (VVar var)]
siStmtExpr e = error $ "selectInstr.siStmtExpr NOT_IMPL e: " <> show e

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

-- Translate print function
-- In the syntax 'print' is used. In the runtime however, the function is called 'print_int.
-- Hence we have to translate somewhere...
fixFuncName :: String -> String 
fixFuncName "print"  = "print_int" 
fixFuncName "getInt" = "read_int"
fixFuncName p = p
