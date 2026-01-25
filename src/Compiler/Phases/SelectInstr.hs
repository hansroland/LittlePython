module Compiler.Phases.SelectInstr where

import Compiler.Syntax

selectInstr :: MProg -> ProgAsmV
selectInstr stmts = concat $ siStmt <$> stmts

siStmt :: MStmt -> [InstrVar]
siStmt (MStmtCall fun atoms )  = siStmtCall fun atoms 
siStmt (MStmtAssign fun expr) = siStmtAssign fun expr
siStmt (MStmtExpr e ) = siStmtExpr e

-- TODO: More than 6 call arguments
siStmtCall :: String -> [MAtom] -> [InstrVar]
siStmtCall fun atoms = parmInstr <> [Instr0 (Callq (fixFuncName fun) arity )]
  where 
    -- pair the atom with the argument corresponding argument reg
    pairs :: [(MAtom, AsmVOp)]
    pairs = zip atoms argumentPassingRegs
    parmInstr :: [InstrVar]
    parmInstr = siStmtCallAtom <$> pairs
    arity = length atoms 
    siStmtCallAtom :: (MAtom, AsmVOp) -> InstrVar
    siStmtCallAtom (MAtomVar v, vreg) = Instr2 Movq (VVar v) vreg
    siStmtCallAtom (MAtomInt n, vreg) = Instr2 Movq (VImm n) vreg

siStmtAssign :: String -> MExpr -> [InstrVar]
siStmtAssign v (MExprAtom a ) = [Instr2 Movq (fromAtom a) (VVar v)]
siStmtAssign v (MExprBinOp op atom1 atom2) = 
    binop op (fromAtom atom1) (fromAtom atom2) (VVar v) 
siStmtAssign v (MExprUOp uop a) = umop uop (fromAtom a) (VVar v)
siStmtAssign v (MExprFunc _ fun atoms ) =   -- TODO Create instuctions for every parameter !!
    [Instr0 (Callq (fixFuncName fun) (length atoms))     
    , Instr2 Movq (VReg Rax)(VVar v) ]

siStmtExpr :: MExpr -> [InstrVar]
siStmtExpr (MExprFunc var fun atoms ) =    -- TODO Add arguments
    [Instr0 (Callq (fixFuncName fun) (length atoms))
    , Instr2 Movq (VReg Rax) (VVar var)]
siStmtExpr e = error $ "selectInstr.siStmtExpr NOT_IMPL e: " <> show e

binop :: BinOp -> AsmVOp -> AsmVOp -> AsmVOp -> [InstrVar]
binop Add op1 op2 res 
    | op2 == res = [Instr2 Addq op1 res]
    | op1 == res = [Instr2 Addq op2 res]
    | otherwise =  [Instr2 Movq op1 res, Instr2 Addq op2 res]      -- res = op1 + op2
binop Sub op1 op2 res 
    | otherwise =  [Instr2 Movq op1 res, Instr2 Subq op2 res]      -- var = op1 -op2

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
