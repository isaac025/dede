module Syntax where

import Data.Set (Set)
import Token (TokenType (..))

data Expr
    = Binary Expr TokenType Expr
    | Unary TokenType Expr
    | Lit Literal
    deriving (Show)

data Literal
    = LBool Bool
    | LNum TokenType
    | LVar TokenType
    | LSet (Set Literal)
    deriving (Show, Eq, Ord)
