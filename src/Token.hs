module Token where

import Data.Text (Text, unpack)

data TokenType
    = -- Single char tokens
      LParen
    | RParen
    | LBrace
    | RBrace
    | Comma
    | Caret
    | Dot
    | Minus
    | Plus
    | Percent
    | SemiColon
    | Star
    | Tilde
    | Pound
    | -- One or two char tokens
      Eq
    | Colon
    | ColonEq
    | Slash
    | SlashEq
    | SlashSlash
    | Greater
    | GreaterEq
    | EqGreater
    | Less
    | LessEq
    | -- Literals
      Identifier
    | Number
    | -- Keywords
      And
    | E
    | Else
    | Fls -- False
    | For
    | I
    | If
    | In
    | Intersect
    | Or
    | Pi
    | Proc
    | Return
    | Tr -- True
    | While
    | Union
    | EOF
    deriving (Enum, Show, Eq, Ord)

data Token = Token
    { tType :: TokenType
    , tLexeme :: Text
    , tLine :: Int
    }

instance Show Token where
    show Token{..} = show tType ++ " " ++ unpack tLexeme
