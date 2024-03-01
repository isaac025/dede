module Parser where

import Control.Conditional (condM, ifM)
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Data.Set (fromList)
import Data.Text (unpack)
import Syntax
import Token

data Parser = Parser
    { pTokens :: [Token]
    , pCurrent :: Int
    }

data ParseError = ParseError Token String deriving (Show)

type ParserT a = ExceptT ParseError (StateT Parser Identity) a

newtype ParserM a = ParserM {runParserM :: ParserT a}
    deriving (Functor, Applicative, Monad, MonadState Parser, MonadError ParseError)

initParser :: [Token] -> Parser
initParser ts = Parser ts 0

runParser :: ParserM a -> Parser -> Either ParseError a
runParser p ps =
    let (e, _) = runIdentity $ runStateT (runExceptT (runParserM p)) ps
     in e

expression :: ParserM Expr
expression = equality

equality :: ParserM Expr
equality = comparison >>= loop
  where
    loop e = do
        b <- match [SlashEq, Eq]
        if b
            then do
                op <- tType <$> previous
                right <- comparison
                loop (Binary e op right)
            else pure e

match :: [TokenType] -> ParserM Bool
match = foldr (\x -> ifM (check x) (advance >> pure True)) (pure False)

check :: TokenType -> ParserM Bool
check tt = do
    iae <- isAtEnd
    if iae
        then pure False
        else do
            ptt <- tType <$> peek
            pure (ptt == tt)

advance :: ParserM Token
advance = do
    b <- isAtEnd
    if not b
        then incCurrent >> previous
        else previous

incCurrent :: ParserM ()
incCurrent = do
    Parser{..} <- get
    put (Parser{pTokens, pCurrent = pCurrent + 1})

isAtEnd :: ParserM Bool
isAtEnd = do
    t <- tType <$> peek
    pure (t == EOF)

peek :: ParserM Token
peek = do
    Parser{..} <- get
    pure (pTokens !! pCurrent)

previous :: ParserM Token
previous = do
    Parser{..} <- get
    pure (pTokens !! (pCurrent - 1))

comparison :: ParserM Expr
comparison = term >>= loop
  where
    loop e = do
        b <- match [Greater, GreaterEq, Less, LessEq]
        if b
            then do
                op <- tType <$> previous
                right <- term
                loop (Binary e op right)
            else pure e

term :: ParserM Expr
term = factor >>= loop
  where
    loop e = do
        b <- match [Minus, Plus, Or]
        if b
            then do
                op <- tType <$> previous
                right <- factor
                loop (Binary e op right)
            else pure e

factor :: ParserM Expr
factor = unary >>= loop
  where
    loop e = do
        b <- match [Slash, Star, And]
        if b
            then do
                op <- tType <$> previous
                right <- unary
                loop (Binary e op right)
            else pure e

unary :: ParserM Expr
unary = do
    m <- match [Tilde, Minus]
    if m
        then do
            op <- tType <$> previous
            Unary op <$> unary
        else primary

primary :: ParserM Expr
primary =
    condM
        [ (match [Fls], pure (Lit (LBool False)))
        , (match [Tr], pure (Lit (LBool True)))
        , (match [Identifier], previous >>= \x -> pure (Lit (LVar (tLexeme x))))
        , (match [Number], buildNumber)
        , (match [LBrace], buildSet)
        ]

buildNumber :: ParserM Expr
buildNumber = do
    p <- unpack . tLexeme <$> previous
    ifM
        (match [Percent])
        (buildRatio p)
        (completeNumber p)
  where
    completeNumber v =
        if '.' `elem` v
            then pure $ Lit (LDouble (read @Double v))
            else pure $ Lit (LInt (read @Integer v))
    buildRatio v = do
        w <- unpack . tLexeme <$> advance
        pure $ Lit (LRatio (read @Rational (v ++ " % " ++ w)))
buildSet :: ParserM Expr
buildSet = do
    e <- loop []
    pure $ Lit (LSet (fromList e))
  where
    loop :: [Literal] -> ParserM [Literal]
    loop xs =
        condM
            [ (match [Tr, Fls], previous >>= \x -> loop (toLit x : xs))
            , (match [RBrace], pure xs)
            , (match [Identifier], previous >>= \x -> loop (toLit x : xs))
            , (match [Comma], loop xs)
            ]
    toLit :: Token -> Literal
    toLit x =
        case tType x of
            Tr -> LBool True
            Fls -> LBool False
            Identifier -> LVar (tLexeme x)

consume :: TokenType -> ParserM Token
consume tt = ifM (check tt) advance (error "Unexpected token")

parse :: ParserM Expr
parse = expression
