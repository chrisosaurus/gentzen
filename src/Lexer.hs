module Lexer
(
    lexer,
)
where

import Control.Applicative
import Data.Char (isSpace, isAlpha, isAlphaNum)
import Data.Token

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
    where lexer_parts = [ lexer_bottom string
                        , lexer_forall string
                        , lexer_exists string
                        , lexer_turnstyle string
                        , lexer_comma string
                        , lexer_implies string
                        , lexer_and string
                        , lexer_or string
                        , lexer_lparen string
                        , lexer_rparen string
                        , lexer_lcurly string
                        , lexer_rcurly string
                        , lexer_lsquare string
                        , lexer_rsquare string
                        , lexer_plus string
                        , lexer_minus string
                        , lexer_period string
                        , lexer_symbol string
                        ]

trim_space :: String -> String
trim_space (ch:rest) | isSpace ch = trim_space rest
trim_space str = str

lexer_bottom :: String -> Maybe(Token, String)
lexer_bottom ('_':rest) = Just (Bottom, rest)
lexer_bottom ('b':'o':'t':'t':'o':'m':rest) = Just (Bottom, rest)
lexer_bottom _          = Nothing

lexer_forall :: String -> Maybe(Token, String)
lexer_forall ('f':'o':'r':'a':'l':'l':rest) = Just (Forall, rest)
lexer_forall ('∀':rest) = Just (Forall, rest)
lexer_forall _          = Nothing

lexer_exists :: String -> Maybe(Token, String)
lexer_exists ('e':'x':'i':'s':'t':'s':rest) = Just (Exists, rest)
lexer_exists ('∃':rest) = Just (Exists, rest)
lexer_exists _          = Nothing

lexer_turnstyle :: String -> Maybe(Token, String)
lexer_turnstyle ('|':'-':rest) = Just (Turnstyle, rest)
lexer_turnstyle ('⊢':rest) = Just (Turnstyle, rest)
lexer_turnstyle _          = Nothing

lexer_comma :: String -> Maybe(Token, String)
lexer_comma (',':rest) = Just (Comma, rest)
lexer_comma _          = Nothing

lexer_implies :: String -> Maybe(Token, String)
lexer_implies ('-':'>':rest) = Just (Implies, rest)
lexer_implies ('→':rest) = Just(Implies, rest)
lexer_implies _          = Nothing

lexer_and :: String -> Maybe(Token, String)
lexer_and ('^':rest) = Just (And, rest)
lexer_and ('&':rest) = Just (And, rest)
lexer_and ('/':'\\':rest) = Just (And, rest)
lexer_and ('∧':rest) = Just (And, rest)
lexer_and _          = Nothing

lexer_or :: String -> Maybe(Token, String)
lexer_or ('v':rest) = Just (Or, rest)
lexer_or ('V':rest) = Just (Or, rest)
lexer_or ('|':rest) = Just (Or, rest)
lexer_or ('\\':'/':rest) = Just (Or, rest)
lexer_or ('∨':rest) = Just (Or, rest)
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

lexer_lsquare :: String -> Maybe(Token, String)
lexer_lsquare ('[':rest) = Just (LSquare, rest)
lexer_lsquare _          = Nothing

lexer_rsquare :: String -> Maybe(Token, String)
lexer_rsquare (']':rest) = Just (RSquare, rest)
lexer_rsquare _          = Nothing

lexer_plus :: String -> Maybe(Token, String)
lexer_plus ('+':rest) = Just (Plus, rest)
lexer_plus _          = Nothing

lexer_minus :: String -> Maybe(Token, String)
lexer_minus ('-':rest) = Just (Minus, rest)
lexer_minus _          = Nothing

lexer_period :: String -> Maybe(Token, String)
lexer_period ('.':rest) = Just (Period, rest)
lexer_period _          = Nothing

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

