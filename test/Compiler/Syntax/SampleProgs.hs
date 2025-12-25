module Compiler.Syntax.SampleProgs where

import Compiler.Syntax

expr01 :: SExpr
expr01 = SExprBinOp Sub (SExprInt 42) (SExprInt 84)

expr02 :: SExpr
expr02 = SExprBinOp Add expr01 (SExprInt 25) 

expr03 :: SExpr 
expr03 = SExprBinOp Sub (SExprVar "var1") (SExprVar "var2") 

expr04 :: SExpr 
expr04 = SExprBinOp Sub (SExprVar "var3") (SExprVar "var4") 

expr05 :: SExpr
expr05 = SExprBinOp Add expr03 expr04

expr06 :: SExpr 
expr06 = SExprBinOp Add (SExprVar "x") (SExprUOp USub (SExprVar "y"))

prog01 :: SProg
prog01 = SProg [
        SStmtAssign "x" (SExprBinOp Add (SExprInt 42) (SExprUOp USub (SExprInt 10))),
        SStmtCall "print" (SExprVar "x")]

prog02 :: SProg 
prog02 = SProg [
    SStmtAssign "a" (SExprInt 42),
    SStmtAssign "b" (SExprVar "a"), 
    SStmtCall "print" (SExprVar "b")]

prog03 :: SProg 
prog03 = SProg [SStmtAssign "var1" (SExprInt 10),
                 SStmtAssign "var2" (SExprInt 20),
                 SStmtAssign "var3" (SExprInt 30),
                 SStmtAssign "var4" (SExprInt 40),
                 SStmtCall "print" expr05]

prog04 :: SProg
prog04 = SProg [SStmtAssign "x" expr01,
                 SStmtAssign "y" expr02,
                 SStmtCall "print" (SExprBinOp Sub (SExprVar "x") (SExprVar "y"))]

prog05 :: SProg 
prog05 = SProg [SStmtAssign "x" expr01,
                 SStmtAssign "y" expr02,
                 SStmtCall "print" (SExprBinOp Sub (SExprVar "x") (SExprVar "y"))]

prog06 :: SProg 
prog06 = SProg [SStmtAssign "z" expr05]

prog07 :: SProg 
prog07 = SProg [SStmtCall "print" expr06]

prog0 :: SProg 
prog0 = SProg [SStmtCall "print" expr06]