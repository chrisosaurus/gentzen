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
consume_symbol expected (Symbol sym:rest) | expected == sym = Right rest
consume_symbol expected _ = Left $ "Symbol not found: " ++ (show expected)

consume_token :: Token -> [Token] -> Either String [Token]
consume_token expected (tok:rest) | expected == tok = Right rest
consume_token expected _ = Left $ "Token not found: " ++ (show expected)

expect_token :: Token -> [Token] -> Either String [Token]
expect_token expected tokens@(tok:_) | expected == tok = Right tokens
expect_token expected _ = Left $ "Token not found: " ++ (show expected)

expect_empty :: [Token] -> Either String [Token]
expect_empty [] = Right []
expect_empty tokens = Left $ "Expected empty: " ++ (show tokens)

parse_string :: [Token] -> Either String ([Token], String)
parse_string (Symbol s:rest) = Right (rest, s)
parse_string tokens = Left $ "Expected string: " ++ (show tokens)


