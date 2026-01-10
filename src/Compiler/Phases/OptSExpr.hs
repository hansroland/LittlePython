-- A Partial Evaluator
-- The compiler eagerly computes the parts of the program that do not
--    depend on any inputs, a process known as partial evaluation
--    (Jones, Gomard, and Sestoft 1993).

module Compiler.Phases.OptSExpr (
    optimizeSProg
    , optimizeSExpr
    )

  where

import Compiler.Syntax

-- | Optimize Constants:
--   Replace: ExprBinOp Add (ExprInt x) (ExprInt y) => (ExprInt (x+y))
--            ExprBinOp Sub (ExprInt x) (ExprInt y) => (ExprInt (x-y))
optimizeSProg :: SProg -> SProg
optimizeSProg = optUSubProg . optConstSProg

optimizeSExpr :: SExpr -> SExpr
optimizeSExpr = optUSubExpr . optConstSExpr

optConstSProg :: SProg -> SProg
optConstSProg (SProg body) = SProg (optConstStmt <$> body)

optConstStmt :: SStmt -> SStmt
optConstStmt (SStmtCall fun exs) = SStmtCall fun $ optConstSExpr <$> exs
optConstStmt (SStmtAssign v ex) = SStmtAssign v $ optConstSExpr ex
optConstStmt (SStmtExpr ex)  = SStmtExpr $ optConstSExpr ex

optConstSExpr :: SExpr -> SExpr
optConstSExpr (SExprBinOp Add lhs rhs) = optConstAdd (optConstSExpr lhs) (optConstSExpr rhs)
optConstSExpr (SExprBinOp Sub lhs rhs) = optConstSub (optConstSExpr lhs) (optConstSExpr rhs)
optConstSExpr (SExprUOp USub e) = (SExprUOp USub (optConstSExpr e))
optConstSExpr e = e

optConstAdd :: SExpr -> SExpr -> SExpr
optConstAdd (SExprInt n1) (SExprInt n2) = SExprInt (n1 + n2)
optConstAdd e1 e2 = SExprBinOp Add e1 e2

optConstSub :: SExpr -> SExpr -> SExpr
optConstSub (SExprInt n1) (SExprInt n2) = SExprInt (n1 - n2)
optConstSub e1 e2 = SExprBinOp Sub e1 e2


-- | Optimizing the Unary Sub operation (aka negation)
--   Remove double negations
--      Replace: ExprUOp USub (ExprInt n) => (ExprInt -n)
--   Push negation to leafs
--      Replace  ExprUOp USub (ExprBinOp x y) => 
--                      ExprBinOp (ExprUOp USub x) (ExprUOp USub y)
optUSubProg :: SProg -> SProg
optUSubProg (SProg body) = SProg (optUSubStmt <$> body)

optUSubStmt :: SStmt -> SStmt
optUSubStmt (SStmtCall fun exs) = SStmtCall fun $ optUSubExpr <$> exs
optUSubStmt (SStmtAssign s ex) = SStmtAssign s $ optUSubExpr ex
optUSubStmt (SStmtExpr ex)  = SStmtExpr $ optUSubExpr ex

optUSubExpr :: SExpr -> SExpr
optUSubExpr (SExprUOp USub (SExprInt n)) = SExprInt (-n)
optUSubExpr (SExprUOp USub (SExprUOp USub e)) = e
optUSubExpr (SExprUOp USub (SExprBinOp op x y)) = 
    SExprBinOp op (SExprUOp USub  x) (SExprUOp USub  y)
optUSubExpr e = e
