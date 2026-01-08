-- This module contains Syntax definitions, that don't change between
--    the differen languages.

module Compiler.Syntax.LangBase where

class PP a where 
    pp :: a -> String

instance (PP a) => PP (Either String a) where 
    pp (Left e) = ("Error: " <> e)
    pp (Right a) = pp a

instance (PP a) => PP [a] where 
    pp [] = ""
    pp [a] = pp a
    pp (a : as) = concat [pp a, "\n", pp as]

instance (PP a, PP b) => PP (a,b) where 
    pp (a,b) = concat ["(", pp a, ", ", pp b, ")"]

data BinOp = Add | Sub
    deriving (Show, Eq)

data UnaryOp = USub
    deriving (Show, Eq)

instance PP BinOp where 
    pp Add = " + "
    pp Sub = " - "

instance PP UnaryOp where 
    pp USub = " -"

