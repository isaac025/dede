module Interpreter where

import Syntax
import Token

isTruthy :: Literal -> Bool
isTruthy (LBool _) = True
isTruthy _ = False

visit :: Expr -> Literal
visit (Binary e1 op e2) = visitBinary
visit (Unary op e) = visitUnary op (visit e)
visit (Lit l) = l

visitUnary :: TokenType -> Literal -> Literal
visitUnary op e =
    case op of
        Minus -> neg e

neg :: Literal -> Literal
neg (LInt l) = LInt (negate l)
neg (LDouble l) = LDouble (negate l)
neg (LRatio l) = LRatio (negate l)
neg _ = error "not a number"

{-
isVariable :: Expr -> Bool
isVariable = undefined

sameVariable :: Expr -> Expr -> Bool
sameVariable = undefined

addend :: Expr -> Expr
addend = undefined

augend :: Expr -> Expr
augend = undefined

makeSum :: Expr -> Expr
makeSum = undefined

isProduct :: Expr -> Bool
isProduct = undefined

multiplier :: Expr -> Expr
multiplier = undefined

multiplicand :: Expr -> Expr
multiplicand = undefined

makeProduct :: Expr -> Expr
makeProduct = undefined

deriv :: Expr -> String -> Expr
deriv = undefined
-}
