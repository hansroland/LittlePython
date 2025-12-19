-- Abstract Syntax Tree for the language to compile : LangInt

module Compiler.Syntax.LangSrc where

import Compiler.Syntax.LangBase

data SExpr =
    SExprInt !Int
    | SExprBinOp !BinOp !SExpr !SExpr
    | SExprUOp !UnaryOp !SExpr
    | SExprCall !String ![SExpr]
  deriving (Eq, Show)

data SStmt = SStmtPrint !SExpr
          | SStmtExpr  !SExpr
  deriving (Eq, Show)

data SProgr = SProgr ![SStmt]
  deriving (Eq, Show)

-- Define a pretty printer. It produces an easy and quick string of a sentence.
--    (This is more ore less the input to the compiler...)
pprint :: SProgr -> String
pprint (SProgr stmts) = concat $ pp <$> stmts

instance PP SStmt where        
    pp (SStmtPrint e) = concat ["print ", pp e]
    pp (SStmtExpr e) = pp e

instance PP SExpr where
    pp (SExprInt n)
      | n >= 0    = show n
      | otherwise = concat ["(", show n, ")"]
    pp (SExprBinOp Add e1 e2) = concat [ "(", pp e1, " + ", pp e2, ")" ]
    pp (SExprBinOp Sub e1 e2) = concat [ "(", pp e1, " - ", pp e2, ")" ]
    pp (SExprUOp USub e)      = concat ["-", pp e]
    pp (SExprCall name args)  = concat ["Call ", name, prtargs]
        where prtargs = concat $ pp <$> args
