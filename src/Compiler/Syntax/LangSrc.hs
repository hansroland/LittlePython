-- Abstract Syntax Tree for the language to compile : LangInt

module Compiler.Syntax.LangSrc where

import Compiler.Syntax.LangBase

data SExpr =
    SExprInt !Int
    | SExprVar !String
    | SExprBinOp !BinOp !SExpr !SExpr
    | SExprUOp !UnaryOp !SExpr
    | SExprCall !String ![SExpr]
  deriving (Eq, Show)

data SStmt = SStmtCall !String !SExpr
          | SStmtAssign !String !SExpr
          | SStmtExpr  !SExpr
  deriving (Eq, Show)

data SProg = SProg ![SStmt]
  deriving (Eq, Show)

-- Define a pretty printer. It produces an easy and quick string of a sentence.
--    (This is more ore less the input to the compiler...)
instance PP SProg where
  pp (SProg stmts) = pp stmts

instance PP SStmt where  
    pp (SStmtCall fun (SExprInt n)) = concat [fun, " (", show n, ")"]
    pp (SStmtCall fun (SExprVar v)) = concat [fun, " (", v, ")"]
    pp (SStmtCall fun e) = concat [fun, " ", pp e]
    pp (SStmtAssign s e) = concat [s, " = ", pp e]
    pp (SStmtExpr e) = pp e

instance PP SExpr where
    pp (SExprInt n)
      | n >= 0    = show n
      | otherwise = concat ["(", show n, ")"]
    pp (SExprVar v)           = v 
    pp (SExprBinOp Add e1 e2) = concat [ "(", pp e1, " + ", pp e2, ")" ]
    pp (SExprBinOp Sub e1 e2) = concat [ "(", pp e1, " - ", pp e2, ")" ]
    pp (SExprUOp USub e)      = concat ["(-", pp e, ")"]
    pp (SExprCall name args)  = concat ["Call ", name, prtargs]
        where prtargs = concat $ pp <$> args

-- Helper Functions
isSLeaf :: SExpr -> Bool 
isSLeaf (SExprInt _) = True 
isSLeaf (SExprVar _) = True
isSLeaf (SExprBinOp _ _ _) = False
isSLeaf (SExprUOp _ _) = False
isSLeaf (SExprCall _ _) = False 
