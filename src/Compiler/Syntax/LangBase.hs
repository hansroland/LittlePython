-- This module contains Syntax definitions, that don't change between
--    the differen languages.

module Compiler.Syntax.LangBase where

class PP a where 
    pp :: a -> String

data BinOp = Add | Sub
    deriving (Show, Eq)

data UnaryOp = USub
    deriving (Show, Eq)

instance PP BinOp where 
    pp Add = " + "
    pp Sub = " - "

instance PP UnaryOp where 
    pp USub = " -"