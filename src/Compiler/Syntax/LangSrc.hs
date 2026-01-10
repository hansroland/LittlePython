-- Abstract Syntax Tree for the language to compile : LangInt

module Compiler.Syntax.LangSrc where

import Compiler.Syntax.LangBase
import Data.List (intercalate)

data SExpr =
    SExprInt !Int
    | SExprVar !String
    | SExprBinOp !BinOp !SExpr !SExpr
    | SExprUOp !UnaryOp !SExpr      
    | SExprFunc !String ![SExpr]
  deriving (Eq, Show)

data SStmt = SStmtCall !String ![SExpr]
          | SStmtAssign !String !SExpr
          | SStmtExpr  !SExpr         -- TODO: Support multiple parameters.
  deriving (Eq, Show)

data SProg = SProg ![SStmt]
  deriving (Eq, Show)

-- Define a pretty printer. It produces an easy and quick string of a sentence.
--    (This is more ore less the input to the compiler...)
instance PP SProg where
  pp (SProg stmts) = pp stmts

instance PP SStmt where  
    pp (SStmtCall fun exs) = concat [fun, " ", pp exs]
    pp (SStmtAssign s e) = concat [s, " = ", pp e]
    pp (SStmtExpr e) = ppc e 

-- Print top level nodes without parens 
-- Print lower level nodes with conditional parens using ppc
instance PP SExpr where
    pp (SExprInt n)
      | n >= 0    = show n
      | otherwise = concat ["(", show n, ")"]
    pp (SExprVar v)           = v 
    pp (SExprBinOp Add e1 e2) = concat [ ppc e1, " + ", ppc e2 ]
    pp (SExprBinOp Sub e1 e2) = concat [ ppc e1, " - ", ppc e2 ]
    pp (SExprUOp USub e)      = concat ["-", ppc e ]
    pp (SExprFunc fun args)   = concat [fun, " ", pp args]

-- | Pretty print conditional
--    Print leaf nodes without parens
--    Print complex nodes with parens 
ppc :: SExpr -> String
ppc (SExprInt n)
    | n >= 0    = show n
    | otherwise = concat ["(", show n, ")"]
ppc (SExprVar v)           = v 
ppc (SExprBinOp Add e1 e2) = concat [ "(", ppc e1, " + ", ppc e2, ")" ]
ppc (SExprBinOp Sub e1 e2) = concat [ "(", ppc e1, " - ", ppc e2, ")" ]
ppc (SExprUOp USub e)      = concat ["(-", ppc e, ")"]
ppc (SExprFunc fun args)   = concat [fun, pp args]

-- Separate a list of SExpr's by commas
instance PP [SExpr] where 
    pp exs = concat ["(", intercalate ", " (pp <$> exs), ")" ]

-- Separate a list of SStm's by newlines
instance PP [SStmt] where 
    pp stmts = intercalate "\n" (pp <$> stmts)

-- Helper Functions
isSLeaf :: SExpr -> Bool 
isSLeaf (SExprInt _) = True 
isSLeaf (SExprVar _) = True
isSLeaf (SExprBinOp _ _ _) = False
isSLeaf (SExprUOp _ _) = False
isSLeaf (SExprFunc _ _) = False 
