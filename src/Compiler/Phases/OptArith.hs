-- A Partial Evaluator
-- The compiler eagerly computes the parts of the program that do not
--    depend on any inputs, a process known as partial evaluation
--    (Jones, Gomard, and Sestoft 1993).

module Compiler.Phases.OptArith (
    optimizeSProg
    , optimizeSExpr
    )

  where

import Compiler.Syntax

-- Optimize Constants:
--   Replace: ExprBinOp Add (ExprInt x) (ExprInt y) => (ExprInt (x+y))
--            ExprBinOp Sub (ExprInt x) (ExprInt y) => (ExprInt (x-y))
optimizeSProg :: SProgr -> SProgr
optimizeSProg = optUSubProgr . optConstSProgr

optimizeSExpr :: SExpr -> SExpr
optimizeSExpr = optUSubExpr . optConstSExpr

optConstSProgr :: SProgr -> SProgr
optConstSProgr (SProgr body) = SProgr (optConstStmt <$> body)

optConstStmt :: SStmt -> SStmt
optConstStmt (SStmtPrint expr) = SStmtPrint $ optConstSExpr expr
optConstStmt (SStmtExpr expr)  = SStmtExpr $ optConstSExpr expr

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


-- Optimizing the Unary Sub operation (aka negation)
--   Remove double negations
--   Replace: ExprUOp USub (ExprInt n) => (ExprInt -n)
optUSubProgr :: SProgr -> SProgr
optUSubProgr (SProgr body) = SProgr (optUSubStmt <$> body)

optUSubStmt :: SStmt -> SStmt
optUSubStmt (SStmtPrint expr) = SStmtPrint $ optUSubExpr expr
optUSubStmt (SStmtExpr expr)  = SStmtExpr $ optUSubExpr expr

optUSubExpr :: SExpr -> SExpr
optUSubExpr (SExprUOp USub (SExprInt n)) = SExprInt (-n)
optUSubExpr (SExprUOp USub (SExprUOp USub e)) = e
optUSubExpr e = e