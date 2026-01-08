-- Evaluator (aka Language Interpreter) for the first language defined in LangSrc (aka LangInt).
--
module Compiler.Syntax.EvalLangSrc (evalSProg, evalSStmt) where

import Compiler.Syntax.LangBase
import Compiler.Syntax.LangSrc

import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import Data.Maybe ( fromJust )

data EvalState = EvalState (Map String Int)
type EvalMonad a = StateT EvalState IO a

initialState :: EvalState
initialState = EvalState Map.empty

evalSProg :: SProg -> IO ()
evalSProg (SProg stmts) = do
    _ <- execStateT (sequence_ (map evalSStmt stmts)) initialState
    pure ()

evalSStmt :: SStmt -> EvalMonad ()
evalSStmt (SStmtCall fun e) = do
    case fun of 
      "print" -> do
        n <- evalSExpr e
        liftIO $ putStrLn $ show n
      _ -> error ("EvalLangSrc.hs Function not implemented: " <> fun)
evalSStmt (SStmtExpr e) = do
    _ <- evalSExpr e
    pure ()
evalSStmt (SStmtAssign var e) = do
    (EvalState vars) <- get
    n <- evalSExpr e
    let nVars = Map.insert var n vars
    put $ EvalState nVars
    return ()
-- This is a classic interpreter for a standard ADT Syntax tree.
-- It runs in the EvalMonad and allows us to keep IO out of the Syntax definition
-- type classes used to define our language.
evalSExpr :: SExpr -> EvalMonad Int
evalSExpr (SExprInt n) = pure n
evalSExpr (SExprUOp USub expr) = do
    e <- evalSExpr expr
    pure $ negate e
evalSExpr (SExprBinOp op exp1 exp2) = do
    e1 <- evalSExpr exp1
    e2 <- evalSExpr exp2
    pure (case op of
           Add -> e1 + e2
           Sub -> e1 - e2)
evalSExpr (SExprFunc "getInt" _ ) = liftIO $ ((read <$> getLine) :: IO Int)  -- TODO add arguments
evalSExpr (SExprVar var) = do                 -- Lookup int value of variable
        (EvalState vars) <- get
        let mb = Map.lookup var vars
        let n = fromJust mb
        pure n
evalSExpr e = error ("Error on evaluating expression: " ++ pp e)
