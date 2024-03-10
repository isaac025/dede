module Scanner (runScan, scanTokens, initScanner, Scanner (..)) where

import Control.Conditional (cond, ifM)
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Maybe (fromMaybe)
import Token

data ScanError = ScanError
    { seLine :: Int
    , seMsg :: String
    }

type ScannerT a = ExceptT ScanError (StateT Scanner Identity) a

data Scanner = Scanner
    { sSource :: String
    , sTokens :: [Token]
    , sStart :: Int
    , sCurrent :: Int
    , sLine :: Int
    , hasError :: Bool
    }
    deriving (Show)

newtype ScanM a = ScanM {runScanM :: ScannerT a}
    deriving (Functor, Applicative, Monad, MonadState Scanner, MonadError ScanError)

runScan :: Scanner -> ScanM a -> (Either ScanError a, Scanner)
runScan st s = runIdentity $ runStateT (runExceptT (runScanM s)) st

printError :: ScanError -> IO ()
printError (ScanError{..}) = print $ "[line " ++ line ++ "] Error: " ++ seMsg
  where
    line = show seLine

type SymTable = [(String, TokenType)]

keywords :: SymTable
keywords =
    [ ("and", And)
    , ("e", E)
    , ("else", Else)
    , ("false", Fls)
    , ("for", For)
    , ("I", I)
    , ("if", If)
    , ("in", In)
    , ("intersect", Intersect)
    , ("or", Or)
    , ("pi", Pi)
    , ("proc", Proc)
    , ("return", Return)
    , ("true", Tr)
    , ("union", Union)
    , ("while", While)
    ]

initScanner :: String -> Scanner
initScanner s = Scanner s [] 0 0 1 False

scanTokens :: ScanM ()
scanTokens = loop
  where
    loop = do
        b <- isAtEnd
        if not b
            then update >> scanToken >> loop
            else do
                st <- get
                put (st{sTokens = sTokens st ++ [eof $ sLine st]})
    eof = Token EOF ""
    update = do
        Scanner{..} <- get
        put (Scanner{sSource, sTokens, sStart = sCurrent, sCurrent, sLine, hasError})

isAtEnd :: ScanM Bool
isAtEnd = do
    Scanner{..} <- get
    pure $ sCurrent >= length sSource

scanToken :: ScanM ()
scanToken = do
    c <- advance
    case c of
        '(' -> addToken LParen
        ')' -> addToken RParen
        '{' -> addToken LBrace
        '}' -> addToken RBrace
        ',' -> addToken Comma
        '^' -> addToken Caret
        '.' -> addToken Dot
        '-' -> addToken Minus
        '+' -> addToken Plus
        '%' -> addToken Percent
        ';' -> addToken SemiColon
        '*' -> addToken Star
        '#' -> scanComment
        '=' -> ifM (match '>') (addToken EqGreater) (addToken Eq)
        '~' -> addToken Tilde
        '/' -> ifM (match '=') (addToken SlashEq) (addToken Slash)
        ':' -> ifM (match '=') (addToken ColonEq) (addToken Colon)
        '>' -> ifM (match '=') (addToken GreaterEq) (addToken Greater)
        '<' -> ifM (match '=') (addToken LessEq) (addToken Less)
        ' ' -> pure ()
        '\t' -> pure ()
        '\n' -> void incLine
        u ->
            cond
                [ (isDigit c, number)
                , (isAlpha c, identifier)
                , (otherwise, void $ unexpectedChar u)
                ]

unexpectedChar :: Char -> ScanM Token
unexpectedChar c = get >>= \s -> throwError $ ScanError (sLine s) ("Unexpected character: " ++ ['\'', c, '\''])

advance :: ScanM Char
advance = do
    Scanner{..} <- get
    put (Scanner{sSource, sTokens, sStart, sCurrent = sCurrent + 1, sLine, hasError})
    pure (sSource !! sCurrent)

addToken :: TokenType -> ScanM ()
addToken tt = do
    Scanner{..} <- get
    let text = substring sStart sCurrent sSource
        newToken = Token{tType = tt, tLexeme = text, tLine = sLine}
    put $ Scanner{sSource, sTokens = sTokens ++ [newToken], sStart, sCurrent, sLine, hasError}

substring :: Int -> Int -> String -> String
substring a b = take (b - a) . drop a

match :: Char -> ScanM Bool
match expected = do
    b <- isAtEnd
    Scanner{..} <- get
    if
        | b -> pure False
        | sSource !! sCurrent /= expected -> pure False
        | otherwise -> incLine >> pure True

incLine :: ScanM ()
incLine = do
    Scanner{..} <- get
    put Scanner{sSource, sTokens, sStart, sCurrent, sLine = sLine + 1, hasError}

scanComment :: ScanM ()
scanComment = loop
  where
    loop = do
        b <- isAtEnd
        p <- peek
        when (p /= '\n' && not b) $ advance >> loop

peek :: ScanM Char
peek = do
    b <- isAtEnd
    Scanner{..} <- get
    if b then pure '\0' else pure (sSource !! sCurrent)

peekNext :: ScanM Char
peekNext = do
    Scanner{..} <- get
    if sCurrent + 1 >= length sSource then pure '\0' else pure $ sSource !! (sCurrent + 1)

munchChars :: (Char -> Bool) -> ScanM ()
munchChars p = do
    c <- peek
    when (p c) (advance >> munchChars p)

number :: ScanM ()
number =
    munchChars isDigit >> do
        p <- peek
        pn <- peekNext
        if p == '.' && isDigit pn
            then advance >> munchChars isDigit >> addToken Number
            else addToken Number

identifier :: ScanM ()
identifier =
    munchChars isAlphaNum >> do
        Scanner{..} <- get
        let text = substring sStart sCurrent sSource
            typ = fromMaybe Identifier (lookup text keywords)
        addToken typ
