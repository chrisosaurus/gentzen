module ParserUtils
(
    consume_symbol,
    consume_token,
    expect_token,
    expect_empty,
    parse_string,
)
where

import Data.Token

consume_symbol :: String -> [Token] -> Either String [Token]
consume_symbol exp (Symbol sym:rest) | exp == sym = Right rest
consume_symbol exp _ = Left $ "Symbol not found: " ++ (show exp)

consume_token :: Token -> [Token] -> Either String [Token]
consume_token exp (tok:rest) | exp == tok = Right rest
consume_token exp _ = Left $ "Token not found: " ++ (show exp)

expect_token :: Token -> [Token] -> Either String [Token]
expect_token exp tokens@(tok:_) | exp == tok = Right tokens
expect_token exp _ = Left $ "Token not found: " ++ (show exp)

expect_empty :: [Token] -> Either String [Token]
expect_empty [] = Right []
expect_empty rem = Left $ "Expected empty: " ++ (show rem)

parse_string :: [Token] -> Either String ([Token], String)
parse_string (Symbol s:rest) = Right (rest, s)
parse_string rem = Left $ "Expected string: " ++ (show rem)


