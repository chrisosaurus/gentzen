module Lexer
(
    lexer,
)
where

import Control.Applicative
import Data.Char (isSpace, isAlpha, isAlphaNum)
import Data.Token

-- each lexing function is of type :: String -> Maybe (Maybe Token, String)
-- the outer maybe represents success/failure of that step
-- the inner maybe being None represents a success with no token production

lexer :: String -> Either String [Token]
lexer ""                     = Right []
lexer (ch:rest) | isSpace ch = lexer (trim_space rest)
lexer string                 =
    case (foldl1 (<|>) lexer_parts) of
         Nothing                  -> Left $ "Failed to parse: " ++ string
         Just (Just token,  [])   -> Right [token]
         Just (Just ltoken, rest) -> case lexer rest of
                                       Left str -> Left str
                                       Right rtokens -> Right (ltoken:rtokens)
         Just (Nothing,     rest) -> case lexer rest of
                                       Left str     -> Left str
                                       Right tokens -> Right tokens
    where lexer_parts = [ lexer_comment string
                        , lexer_bottom string
                        , lexer_forall string
                        , lexer_exists string
                        , lexer_turnstile string
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
                        , lexer_equal string
                        , lexer_lessthan string
                        , lexer_greaterthan string
                        , lexer_forwardslash string
                        , lexer_symbol string
                        ]

trim_space :: String -> String
trim_space (ch:rest) | isSpace ch = trim_space rest
trim_space str = str

lexer_bottom :: String -> Maybe(Maybe Token, String)
lexer_bottom ('_':rest) = Just (Just Bottom, rest)
lexer_bottom ('B':'o':'t':'t':'o':'m':rest) = Just (Just Bottom, rest)
lexer_bottom _          = Nothing

discard_until :: Char -> String -> String
discard_until _  [] = []
discard_until ch (x:xs) | ch == x = xs
discard_until ch (_:xs)           = discard_until ch xs

-- comments go until end of line
lexer_comment :: String -> Maybe(Maybe Token, String)
lexer_comment ('-':'-':rest) = Just (Nothing, discard_until '\n' rest)
lexer_comment _ = Nothing

lexer_forall :: String -> Maybe(Maybe Token, String)
lexer_forall ('f':'o':'r':'a':'l':'l':rest) = Just (Just Forall, rest)
lexer_forall ('∀':rest) = Just (Just Forall, rest)
lexer_forall _          = Nothing

lexer_exists :: String -> Maybe(Maybe Token, String)
lexer_exists ('e':'x':'i':'s':'t':'s':rest) = Just (Just Exists, rest)
lexer_exists ('∃':rest) = Just (Just Exists, rest)
lexer_exists _          = Nothing

lexer_turnstile :: String -> Maybe(Maybe Token, String)
lexer_turnstile ('|':'-':rest) = Just (Just Turnstile, rest)
lexer_turnstile ('⊢':rest) = Just (Just Turnstile, rest)
lexer_turnstile _          = Nothing

lexer_comma :: String -> Maybe(Maybe Token, String)
lexer_comma (',':rest) = Just (Just Comma, rest)
lexer_comma _          = Nothing

lexer_implies :: String -> Maybe(Maybe Token, String)
lexer_implies ('-':'>':rest) = Just (Just Implies, rest)
lexer_implies ('→':rest) = Just(Just Implies, rest)
lexer_implies _          = Nothing

lexer_and :: String -> Maybe(Maybe Token, String)
lexer_and ('^':rest) = Just (Just And, rest)
lexer_and ('&':rest) = Just (Just And, rest)
lexer_and ('/':'\\':rest) = Just (Just And, rest)
lexer_and ('∧':rest) = Just (Just And, rest)
lexer_and _          = Nothing

lexer_or :: String -> Maybe(Maybe Token, String)
lexer_or ('v':rest) = Just (Just Or, rest)
lexer_or ('V':rest) = Just (Just Or, rest)
lexer_or ('|':rest) = Just (Just Or, rest)
lexer_or ('\\':'/':rest) = Just (Just Or, rest)
lexer_or ('∨':rest) = Just (Just Or, rest)
lexer_or _          = Nothing

lexer_lparen :: String -> Maybe(Maybe Token, String)
lexer_lparen ('(':rest) = Just (Just LParen, rest)
lexer_lparen _          = Nothing

lexer_rparen :: String -> Maybe(Maybe Token, String)
lexer_rparen (')':rest) = Just (Just RParen, rest)
lexer_rparen _          = Nothing

lexer_lcurly :: String -> Maybe(Maybe Token, String)
lexer_lcurly ('{':rest) = Just (Just LCurly, rest)
lexer_lcurly _          = Nothing

lexer_rcurly :: String -> Maybe(Maybe Token, String)
lexer_rcurly ('}':rest) = Just (Just RCurly, rest)
lexer_rcurly _          = Nothing

lexer_lsquare :: String -> Maybe(Maybe Token, String)
lexer_lsquare ('[':rest) = Just (Just LSquare, rest)
lexer_lsquare _          = Nothing

lexer_rsquare :: String -> Maybe(Maybe Token, String)
lexer_rsquare (']':rest) = Just (Just RSquare, rest)
lexer_rsquare _          = Nothing

lexer_plus :: String -> Maybe(Maybe Token, String)
lexer_plus ('+':rest) = Just (Just Plus, rest)
lexer_plus _          = Nothing

lexer_minus :: String -> Maybe(Maybe Token, String)
lexer_minus ('-':rest) = Just (Just Minus, rest)
lexer_minus _          = Nothing

lexer_period :: String -> Maybe(Maybe Token, String)
lexer_period ('.':rest) = Just (Just Period, rest)
lexer_period _          = Nothing

lexer_equal :: String -> Maybe(Maybe Token, String)
lexer_equal ('=':rest) = Just (Just Equal, rest)
lexer_equal _          = Nothing

lexer_lessthan :: String -> Maybe(Maybe Token, String)
lexer_lessthan ('<':rest) = Just (Just LessThan, rest)
lexer_lessthan _          = Nothing

lexer_greaterthan :: String -> Maybe(Maybe Token, String)
lexer_greaterthan ('>':rest) = Just (Just GreaterThan, rest)
lexer_greaterthan _          = Nothing

lexer_forwardslash :: String -> Maybe(Maybe Token, String)
lexer_forwardslash ('/':rest) = Just (Just ForwardSlash, rest)
lexer_forwardslash _          = Nothing


lexer_symbol :: String -> Maybe (Maybe Token, String)
lexer_symbol string = lexer_symbol' string ""

--             input     symbol so far
lexer_symbol' :: String -> String -> Maybe (Maybe Token, String)
lexer_symbol'   (ch:rest)  ""     | should_discard ch = lexer_symbol' rest ""
--                                  first must be alpha
lexer_symbol'   (ch:rest)  ""     | isAlpha ch        = lexer_symbol' rest [ch]
lexer_symbol'   (ch:rest)  ""                         = Nothing
lexer_symbol'   (ch:rest)  sym    | should_break ch   = Just (Just (Symbol sym), ch:rest)
--                                  following can be alphaNum or underscore
lexer_symbol'   (ch:rest)  sym    | isAlphaNum ch     = lexer_symbol' rest (sym++[ch])
lexer_symbol'   (ch:rest)  sym    | ch == '_'         = lexer_symbol' rest (sym++[ch])
lexer_symbol'   []         ""                         = Nothing
lexer_symbol'   rest       sym                        = Just (Just (Symbol sym), rest)


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
should_break '<'             = True
should_break '>'             = True
should_break _               = False

