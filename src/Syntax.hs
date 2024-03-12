module Syntax where

import Data.List (intercalate)
import Data.Set (Set, toList)
import Token (TokenType (..))

data Expr
    = Binary Expr TokenType Expr
    | Unary TokenType Expr
    | Lit Literal
    deriving (Eq)

instance Show Expr where
    show (Binary e1 op e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
    show (Unary op e) = show op ++ show e
    show (Lit l) = show l

data Literal
    = LBool Bool
    | LInt Integer
    | LDouble Double
    | LRatio Rational
    | LVar String
    | LSet (Set Literal)
    deriving (Eq, Ord)

instance Show Literal where
    show (LBool True) = "true"
    show (LBool False) = "false"
    show (LInt n) = show n
    show (LDouble d) = show d
    show (LRatio r) = show r
    show (LVar x) = x
    show (LSet s) = "{" ++ s' ++ "}"
      where
        s' = intercalate ", " $ map show (toList s)
