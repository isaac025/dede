module Syntax where

import Data.Set (Set)
import Data.Text (Text)
import Token (TokenType (..))

data Expr
    = Binary Expr TokenType Expr
    | Unary TokenType Expr
    | Lit Literal
    deriving (Show)

data Literal
    = LBool Bool
    | LInt Integer
    | LDouble Double
    | LRatio Rational
    | LVar Text
    | LSet (Set Literal)
    deriving (Show, Eq, Ord)
