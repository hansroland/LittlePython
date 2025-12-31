module Compiler.Phases.Parser (parseLpy) where

import Compiler.Syntax

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Monad

type Parser = Parsec Void String

-- Lexer functions

-- Space Consumer
sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "#"
        blockCmnt = L.skipBlockComment "\"\"\"" "\"\"\"" 

-- Strategy: Whitespaces will be consumed after every lexeme automatically, 
--   but not before it.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc 

-- symbol - parse a given string aka a symbol 
symbol :: String -> Parser String
symbol = L.symbol sc

-- parens - Parse something between parenthesis
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")") 

-- integer - parse an int  (Note: decimal is polymorphic in it's Num type)
integer :: Parser Int
integer = lexeme L.decimal 

-- Expression parser 
aExpr :: Parser SExpr
aExpr = makeExprParser aTerm table <?> "expression"

-- aTerm - Arithmetic term parser
aTerm :: Parser SExpr
aTerm = parens aExpr
  <|> SExprVar <$> identifier
  <|> SExprInt <$> integer 


-- Table to define the expression. 
--   The inner lists are in descending precedence
table :: [[Operator Parser SExpr]]
table =  [[ prefix  "-"  (SExprUOp USub)]
--         , prefix  "+"  id ]
--         , [ postfix "++" (+1) ]
--         , [ binary  "*"  (*)
--        , binary  "/"  div  ]
         , [ binary  "+"  (SExprBinOp Add)
           ,binary  "-"  (SExprBinOp Sub) ] ]
  where
    binary  name f = InfixL  (f <$ symbol name)
    prefix  name f = Prefix  (f <$ symbol name)
    -- postfix name f = Postfix (f <$ symbol name)

-- identifier - parse a variable name
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

-- rword - parse a reserved word (aka keywordS)       (NOT YET USED)
-- rword :: String -> Parser ()
-- rword w = string w *> notFollowedBy alphaNumChar *> sc

rws :: [String] 
rws = [
    "False",    "def",      "if",       "raise",    
    "None",     "del",      "import",   "return",
    "True",     "elif",     "in",       "try", 
    "and",      "else",     "is",       "while",
    "as",       "except",   "lambda",   "with",
    "assert",   "finally",  "nonlocal", "yield",
    "break",    "for",      "not",      "class", 
    "form",     "or",       "continue", "global",
    "pass" ]

-- pStmt - a parser for statements 
--       - expression statements are still missing!!!
pStmt :: Parser SStmt
pStmt = try pStmtAssign <|> pStmtCall 
  where
    pStmtAssign :: Parser SStmt
    pStmtAssign = do
        v <- identifier
        void $ symbol "="
        ex <- aExpr 
        pure $ SStmtAssign v ex

    pStmtCall :: Parser SStmt 
    pStmtCall = do
        fun <- identifier
        ex  <- parens aExpr 
        pure $ SStmtCall fun ex 

-- A parser for the whole program
parseProg :: Parser SProg 
parseProg = do  
  stmts <- some pStmt 
  return $ SProg stmts 

parseLpy :: FilePath -> String -> Either String SProg 
parseLpy name input = 
  case runParser parseProg name input of 
    Left bundle -> Left $ show $ errorBundlePretty bundle
    Right p -> Right p
