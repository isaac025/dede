module Token where

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
    | SlashSlash
    | SlashEq
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
    deriving (Enum, Eq, Ord)

instance Show TokenType where
    show LParen = "("
    show RParen = ")"
    show LBrace = "{"
    show RBrace = "}"
    show Comma = ","
    show Caret = "^"
    show Dot = "."
    show Minus = "-"
    show Plus = "+"
    show Percent = "%"
    show SemiColon = ";"
    show Star = "*"
    show Tilde = "~"
    show Eq = "="
    show Colon = ":"
    show ColonEq = ":="
    show Slash = "/"
    show SlashEq = "/="
    show Greater = ">"
    show GreaterEq = ">="
    show EqGreater = "=>"
    show Less = "<"
    show LessEq = "<="
    show Identifier = ""
    show Number = ""
    show And = "and"
    show E = "e"
    show Else = "else"
    show Fls = "false"
    show For = "for"
    show I = "i"
    show If = "if"
    show In = "in"
    show Intersect = "inter"
    show Or = "or"
    show Pi = "pi"
    show Proc = "proc"
    show Return = "return"
    show Tr = "true"
    show While = "while"
    show Union = "union"
    show EOF = ""
    show Pound = ""

data Token = Token
    { tType :: TokenType
    , tLexeme :: String
    , tLine :: Int
    }

instance Show Token where
    show Token{..} = show tType ++ " " ++ tLexeme
