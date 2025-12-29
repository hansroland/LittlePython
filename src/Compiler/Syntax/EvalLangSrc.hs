-- Evaluator (aka Language Interpreter) for the first language defined in LangSrc (aka LangInt).
--
module Compiler.Syntax.EvalLangSrc (evalSProgr, evalSStmt) where

import Compiler.Syntax.LangBase
import Compiler.Syntax.LangSrc

evalSProgr :: SProgr -> IO ()
evalSProgr (SProgr stmts ) =  mapM_ evalSStmt stmts

evalSStmt :: SStmt -> IO (Int)
evalSStmt (SStmtPrint e) = do
    n <- evalSExpr e
    putStrLn $ show n
    return 0
evalSStmt (SStmtExpr e) = evalSExpr e

-- This is a classic interpreter for a standard ADT Syntax tree.
-- It runs in the IO Monad and allows us to keep IO out of the Syntax definition.
evalSExpr :: SExpr -> IO Int
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
evalSExpr (SExprCall "read_int" []) = (read  <$> getLine) :: IO Int
evalSExpr e = error ("Error on evaluating expression: " ++ show e)