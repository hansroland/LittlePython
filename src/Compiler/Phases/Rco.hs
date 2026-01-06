-- RCO Remove Comple Operands
-- Convert a LangSrc program to a LangSrcMon program 

module Compiler.Phases.Rco
  where

import Compiler.Syntax.LangBase
import Compiler.Syntax.LangSrc  
import Compiler.Syntax.LangSrcMon   

import Control.Monad.State.Strict

type RcoMonad a = State Int a

-- | Convert an expression tree to a list of atomic statements
rco :: SProg-> MProg
rco (SProg lestmts) =
   let atms = evalState (sequence (rcoStmt <$> lestmts)) 0
   in MProg $ concat atms

-- Convert a single statement
rcoStmt :: SStmt -> RcoMonad [MStmt]
rcoStmt (SStmtCall fun e) = rcoStmtCall fun e 
rcoStmt (SStmtExpr e)     = rcoStmtExpr e
rcoStmt (SStmtAssign a e) = rcoStmtAssign a e 

rcoStmtCall :: String -> SExpr -> RcoMonad [MStmt]
rcoStmtCall fun (SExprVar v) = pure $ [MStmtCall fun (MAtomVar v)]
rcoStmtCall fun (SExprInt n) = pure $ [MStmtCall fun (MAtomInt n)]
rcoStmtCall fun e = do 
    newVar <- getAssignVar e
    atms <- rcoExpr e newVar
    pure $ concat [atms, [MStmtCall fun (MAtomVar newVar)]]

rcoStmtExpr :: SExpr -> RcoMonad [MStmt]
rcoStmtExpr (SExprVar v) = pure $ [MStmtExpr (MExprAtom(MAtomVar v))]
rcoStmtExpr (SExprInt n) = pure $ [MStmtExpr (MExprAtom(MAtomInt n))]
rcoStmtExpr e = do
    newVar <- getAssignVar e
    atms <- rcoExpr e newVar  
    pure $ concat [atms, [MStmtExpr (getExpr (last atms))]]

rcoStmtAssign :: String -> SExpr -> RcoMonad [MStmt]
rcoStmtAssign a (SExprVar v) = pure $ [MStmtAssign a (MExprAtom(MAtomVar v))]
rcoStmtAssign a (SExprInt n) = pure $ [MStmtAssign a (MExprAtom(MAtomInt n))]
rcoStmtAssign a e = do 
    atms <- rcoExpr e a
    pure $ init atms ++ [MStmtAssign a (getExpr (last atms))]

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
    
rcoExpr (SExprFunc fun {-es-}) a = do                   -- TODO add funtion arguments
    pure $ [MStmtExpr (MExprFunc a fun)]

    -- pairs <- mapM rcoExprChild es                    -- :t pairs = [(MAtom, [MStmt])]
    -- let atoms = fst <$> pairs  
    -- let stmts = concat $ snd <$> pairs           
    -- let newCall = [MStmtExpr (MExprFunc fun atoms)]
    -- pure $ stmts <> newCall 

-- convert a child of an expression, eg: rhs, lhs, unop-argument
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

-- Return the assignment variable of a source expression    
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
