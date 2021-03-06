module Data.Token
(
    Token(..),
)
where

data Token = Turnstile
           | Comma
           | Implies
           | And
           | Or
           | LParen
           | RParen
           | LCurly
           | RCurly
           | LSquare
           | RSquare
           | Symbol String
           | Bottom
           | Forall
           | Exists
           | Plus
           | Minus
           | Period
           | Equal
           | LessThan
           | GreaterThan
           | ForwardSlash
    deriving (Eq)

stringify :: Token -> String
stringify Turnstile = "|-"
stringify Comma = ","
stringify Implies = "->"
stringify And = "^"
stringify Or = "v"
stringify LParen = "("
stringify RParen = ")"
stringify LCurly = "{"
stringify RCurly = "}"
stringify LSquare = "["
stringify RSquare = "]"
stringify (Symbol s) = s
stringify Bottom  = "_"
stringify Forall = "forall"
stringify Exists = "exists"
stringify Plus = "+"
stringify Minus = "-"
stringify Period = "."
stringify Equal = "="
stringify LessThan = "<"
stringify GreaterThan = ">"
stringify ForwardSlash = "/"

instance Show Token where
  show token = stringify token


