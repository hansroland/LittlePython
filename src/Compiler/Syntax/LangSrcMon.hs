-- Abstract Syntax tree for the monadic version of LangVar
module Compiler.Syntax.LangSrcMon  where

import Compiler.Syntax.LangBase
import Data.List (intercalate)

-- A program of our object language
data MProg = MProg ![MStmt]
  deriving (Eq, Show)

data MStmt = MStmtCall !String !MAtom    -- print_int !!
          | MStmtAssign !String !MExpr
          | MStmtExpr !MExpr
  deriving (Eq, Show)

data MExpr =
    MExprAtom !MAtom
    | MExprBinOp !BinOp !MAtom !MAtom
    | MExprUOp !UnaryOp !MAtom
    | MExprCall !String ![MAtom]        -- read_int  !!
  deriving (Show, Eq)

data MAtom =
    MAtomInt !Int
    | MAtomVar !String
  deriving (Show, Eq)

instance PP MProg  where 
  pp (MProg stmts) = pp stmts

instance PP MStmt where 
  pp (MStmtCall fun ex) = concat [fun, " ", pp ex]
  pp (MStmtAssign assign ex) = concat [assign, " = ", pp ex]
  pp (MStmtExpr ex) = pp ex

instance PP MExpr where
  pp (MExprAtom atom) = pp atom
  pp (MExprBinOp binop lhs rhs) = concat [pp lhs, pp binop, pp rhs]
  pp (MExprUOp unop atom) = concat [pp unop, pp atom]
  pp (MExprCall fun atms) = concat ["call ", fun, "(", intercalate "," (pp <$> atms), ")"]        

instance PP MAtom where
  pp (MAtomInt n) = show n
  pp (MAtomVar var) = var
