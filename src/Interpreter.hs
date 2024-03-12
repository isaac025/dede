module Interpreter where

import Syntax
import Token

visit :: Expr -> Literal
visit (Binary e1 op e2) = visitBinary op e1 e2
visit (Unary op e) = visitUnary op (visit e)
visit (Lit l) = l

visitUnary :: TokenType -> Literal -> Literal
visitUnary op e =
    case op of
        Minus -> neg e
        Tilde -> LBool (not $ isTruthy e)

neg :: Literal -> Literal
neg (LInt l) = LInt (negate l)
neg (LDouble l) = LDouble (negate l)
neg (LRatio l) = LRatio (negate l)
neg _ = error "not a number"

isTruthy :: Literal -> Bool
isTruthy (LBool b) = b
isTruthy _ = error "Expected a boolean"

visitBinary :: TokenType -> Expr -> Expr -> Literal
visitBinary op l r =
    case op of
        Minus ->
            if isNum l && isNum r
                then undefined
                else error "Expecting"
        SlashEq -> LBool (l /= r)
        Eq -> LBool (l == r)

isNum :: Expr -> Bool
isNum (Lit (LInt _)) = True
isNum (Lit (LDouble _)) = True
isNum (Lit (LRatio _)) = True
isNum (Lit (LVar _)) = True
isNum _ = False
