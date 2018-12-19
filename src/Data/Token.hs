module Data.Token
(
    Token(..),
)
where

data Token = Turnstyle
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
    deriving (Eq)

stringify :: Token -> String
stringify Turnstyle = "|-"
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

instance Show Token where
  show token = stringify token


