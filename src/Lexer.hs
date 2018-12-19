module Lexer
(
    Token (..),
    lexer,
)
where

import Control.Applicative
import Data.Char (isSpace, isAlpha, isAlphaNum)


data Token = Turnstyle
           | Comma
           | Implies
           | And
           | Or
           | LParen
           | RParen
           | LCurly
           | RCurly
           | Symbol String
    deriving (Show, Eq)

lexer :: String -> Either String [Token]
lexer ""                     = Right []
lexer (ch:rest) | isSpace ch = lexer (trim_space rest)
lexer string                 =
    case (foldl1 (<|>) lexer_parts) of
         Nothing                  -> Left $ "Failed to parse: " ++ string
         Just (token, [])         -> Right [token]
         Just (ltoken, remaining) -> case lexer remaining of
                                          Left str -> Left str
                                          Right rtokens -> Right (ltoken:rtokens)
    where lexer_parts = [ lexer_turnstyle string
                        , lexer_comma string
                        , lexer_implies string
                        , lexer_and string
                        , lexer_or string
                        , lexer_lparen string
                        , lexer_rparen string
                        , lexer_rcurly string
                        , lexer_rcurly string
                        , lexer_symbol string
                        ]

trim_space :: String -> String
trim_space (ch:rest) | isSpace ch = trim_space rest
trim_space str = str

lexer_turnstyle :: String -> Maybe(Token, String)
lexer_turnstyle ('|':'-':rest) = Just (Turnstyle, rest)
lexer_turnstyle _          = Nothing

lexer_comma :: String -> Maybe(Token, String)
lexer_comma (',':rest) = Just (Comma, rest)
lexer_comma _          = Nothing

lexer_implies :: String -> Maybe(Token, String)
lexer_implies ('-':'>':rest) = Just (Implies, rest)
lexer_implies _          = Nothing

lexer_and :: String -> Maybe(Token, String)
lexer_and ('^':rest) = Just (And, rest)
lexer_and ('&':rest) = Just (And, rest)
lexer_and _          = Nothing

lexer_or :: String -> Maybe(Token, String)
lexer_or ('v':rest) = Just (Or, rest)
lexer_or ('V':rest) = Just (Or, rest)
lexer_or ('|':rest) = Just (Or, rest)
lexer_or _          = Nothing

lexer_lparen :: String -> Maybe(Token, String)
lexer_lparen ('(':rest) = Just (LParen, rest)
lexer_lparen _          = Nothing

lexer_rparen :: String -> Maybe(Token, String)
lexer_rparen (')':rest) = Just (RParen, rest)
lexer_rparen _          = Nothing

lexer_lcurly :: String -> Maybe(Token, String)
lexer_lcurly ('{':rest) = Just (LCurly, rest)
lexer_lcurly _          = Nothing

lexer_rcurly :: String -> Maybe(Token, String)
lexer_rcurly ('}':rest) = Just (RCurly, rest)
lexer_rcurly _          = Nothing

lexer_symbol :: String -> Maybe (Token, String)
lexer_symbol string = lexer_symbol' string ""

--             input     symbol so far
lexer_symbol' :: String -> String -> Maybe (Token, String)
lexer_symbol'   (ch:rest)  ""     | should_discard ch = lexer_symbol' rest ""
--                                  first must be alpha
lexer_symbol'   (ch:rest)  ""     | isAlpha ch        = lexer_symbol' rest [ch]
lexer_symbol'   (ch:rest)  ""                         = Nothing
lexer_symbol'   (ch:rest)  sym    | should_break ch   = Just (Symbol sym, ch:rest)
--                                  following can be alphaNum
lexer_symbol'   (ch:rest)  sym    | isAlphaNum ch     = lexer_symbol' rest (sym++[ch])
lexer_symbol'   []         ""                         = Nothing
lexer_symbol'   rest       sym                        = Just (Symbol sym, rest)


should_discard :: Char -> Bool
should_discard ch | isSpace ch = True
should_discard _               = False

should_break :: Char -> Bool
should_break ch | isSpace ch = True
should_break '('             = True
should_break ')'             = True
should_break '['             = True
should_break ']'             = True
should_break '.'             = True
should_break ':'             = True
-- start of arrow
should_break '-'             = True
should_break '\\'            = True
should_break '/'             = True
should_break _               = False

