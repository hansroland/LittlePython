-- This module contains Syntax definitions, that don't change between
--    the differen languages.

module Compiler.Syntax.LangBase where

-- | a Pretty Print instance
class PP a where 
    pp :: a -> String
-- The instance for PP [a] is not defined gnerally.
-- Sometimes we want to separate the itmes with commas,
-- ssometimes with newlines

instance (PP a) => PP (Either String a) where 
    pp (Left e) = ("Error: " <> e)
    pp (Right a) = pp a

instance (PP a, PP b) => PP (a,b) where 
    pp (a,b) = concat ["(", pp a, ", ", pp b, ")"]

instance (PP a) => PP (Maybe a) where 
    pp (Just x) = "Just " <> pp x 
    pp Nothing  = "Nothing"

data BinOp = Add | Sub
    deriving (Show, Eq)

data UnaryOp = USub
    deriving (Show, Eq)

instance PP BinOp where 
    pp Add = " + "
    pp Sub = " - "

instance PP UnaryOp where 
    pp USub = " -"
