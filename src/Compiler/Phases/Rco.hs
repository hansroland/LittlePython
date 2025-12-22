-- RCO Remove Comple Operands
-- Convert a LangSrc program to a LangSrcMon program 

module Compiler.Phases.Rco
  where

import Compiler.Syntax.LangBase
import Compiler.Syntax.LangSrc  
import Compiler.Syntax.LangSrcMon   

import Control.Monad.State.Strict

type RcoMonad a = State Int a

-- Convert an expression tree to a list of atomic statements
rco :: SProg-> MProg
rco (SProg lestmts) =
   let atms = evalState (sequence (rcoStmt <$> lestmts)) 0
   in MProg $ concat atms

-- Convert a single statement
rcoStmt :: SStmt -> RcoMonad [MStmt]
rcoStmt (SStmtCall fun (SExprVar v)) =                     -- print simple variable expr
    pure $ [MStmtCall fun (MAtomVar v)]
rcoStmt (SStmtCall fun (SExprInt n)) =                     -- print simple integer expr
    pure $ [MStmtCall fun (MAtomInt n)]
rcoStmt (SStmtCall fun e) = do                               -- print complex expr
    newVar <- getAssignVar e
    atms <- rcoExpr e newVar
    pure $ concat [atms, [MStmtCall fun (MAtomVar newVar)]]

rcoStmt (SStmtExpr (SExprVar v)) =                     -- simple variable expr
    pure $ [MStmtExpr (MExprAtom(MAtomVar v))]
rcoStmt (SStmtExpr (SExprInt n)) =                     -- simple int expr
    pure $ [MStmtExpr (MExprAtom(MAtomInt n))]
rcoStmt (SStmtExpr e) = do                               -- complex expr
    newVar <- getAssignVar e
    atms <- rcoExpr e newVar  
    pure $ concat [atms, [MStmtExpr (getExpr (last atms))]]

rcoStmt (SStmtAssign a (SExprVar v)) =                     -- assign simple atom variable
    pure $ [MStmtAssign a (MExprAtom(MAtomVar v))]
rcoStmt (SStmtAssign a (SExprInt n)) =                     -- assign seimple int variable   
    pure $ [MStmtAssign a (MExprAtom(MAtomInt n))]
rcoStmt (SStmtAssign s e) = do                              -- assign complex variable
    atms <- rcoExpr e s
    pure $ init atms ++ [MStmtAssign s (getExpr (last atms))]

-- Convert an expression
rcoExpr :: SExpr -> String -> RcoMonad [MStmt]
rcoExpr (SExprInt n) _ = do 
   pure $  [MStmtExpr (MExprAtom (MAtomInt n))]
rcoExpr (SExprVar s) _ = do 
   pure $  [MStmtExpr (MExprAtom (MAtomVar s))]  
rcoExpr (SExprBinOp op lhs rhs) a = do
    (atomLhs, stmtsLhs) <- rcoExprChild lhs
    (atomRhs, stmtsRhs) <- rcoExprChild rhs
    let newBinop = [MStmtAssign a (MExprBinOp op atomLhs atomRhs)]
    pure $ concat $ [stmtsLhs, stmtsRhs, newBinop]
rcoExpr (SExprUOp op ex) a = do
    (atom, stmtsUn) <- rcoExprChild ex
    let newUnop = [MStmtAssign a (MExprUOp op atom)]
    pure $ stmtsUn <> newUnop
    
rcoExpr (SExprCall fun es) _ = do
    pairs <- mapM rcoExprChild es                    -- :t pairs = [(MAtom, [MStmt])]
    let atoms = fst <$> pairs  
    let stmts = concat $ snd <$> pairs           
    let newCall = [MStmtExpr (MExprCall fun atoms)]
    pure $ stmts <> newCall 

-- conver a child of an expression, eg: rhs, lhs, unop-argument
-- Returns the atom of the top level of the expression
--   and a list of statements of all the child statements
rcoExprChild :: SExpr -> RcoMonad (MAtom, [MStmt])
rcoExprChild ch = do 
    case isSLeaf ch of 
      True -> pure $ (toAtom ch, []) 
      False -> do
        assign <- getAssignVar ch
        stmts <- rcoExpr ch assign
        pure $ (MAtomVar assign, stmts)

-- Return the assignment variable of an source expression    
getAssignVar :: SExpr -> RcoMonad String
getAssignVar (SExprVar str) = pure str 
getAssignVar _ = getNewVar
  where
    getNewVar :: RcoMonad String 
    getNewVar = do 
      n <- get 
      put $ n + 1 
      pure $ "tmp_" ++ show n

-- Convert ExprInt or ExprVar to Atoms
toAtom :: SExpr -> MAtom
toAtom (SExprInt n) = MAtomInt n
toAtom (SExprVar v) = MAtomVar v
toAtom e = error ("Rco.hs cannot convert to atom: " ++ show e)

-- Extract the expression from a Stmt
getExpr :: MStmt -> MExpr 
getExpr ex@(MStmtCall _ _) = error ("Rco.getExpr MStmtCall does not contain an expression: " <> pp ex)
getExpr (MStmtAssign _ e) = e
getExpr (MStmtExpr e) = e
