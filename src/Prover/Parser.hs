module Prover.Parser
(
    parse,
)
where

import Lexer
import qualified Sequent.Data.Sequent as Sequent
import qualified Sequent.Parser as Sequent
import Prover.Data.Theorem

consume_symbol :: String -> [Token] -> Either String [Token]
consume_symbol exp (Symbol sym:rest) | exp == sym = Right rest
consume_symbol exp _ = Left $ "Symbol not found: " ++ (show exp)

expect_empty :: [Token] -> Either String [Token]
expect_empty [] = Right []
expect_empty rem = Left $ "Expected empty: " ++ (show rem)

parse_string :: [Token] -> Either String ([Token], String)
parse_string (Symbol s:rest) = Right (rest, s)
parse_string rem = Left $ "Expected string: " ++ (show rem)

parse :: [Token] -> Either String Theorem
parse tokens = do
    (tokens', name) <- parse_name tokens
    (tokens'', system) <- parse_system tokens'
    (tokens''', sequent) <- parse_sequent tokens''
    (tokens'''', steps) <- parse_proof tokens'''
    tokens''''' <- expect_empty tokens''''
    return Theorem {name=name, system=system, sequent=sequent, steps=steps}

parse_name :: [Token] -> Either String ([Token], String)
parse_name tokens = do
    tokens' <- consume_symbol "theorem" tokens
    (tokens'', str) <- parse_string tokens'
    return (tokens'', str)

parse_system :: [Token] -> Either String ([Token], String)
parse_system tokens = do
    tokens' <- consume_symbol "system" tokens
    (tokens'', str) <- parse_string tokens'
    return (tokens'', str)

parse_sequent :: [Token] -> Either String ([Token], Sequent.Sequent)
parse_sequent tokens = do
    tokens' <- consume_symbol "sequent" tokens
    (tokens'', sequent) <- Sequent.parse_prefix tokens'
    return (tokens'', sequent)

parse_proof :: [Token] -> Either String ([Token], [Stmt])
parse_proof tokens = do
    tokens' <- consume_symbol "proof" tokens
    (tokens'', body) <- parse_proof_body tokens'
    tokens''' <- consume_symbol "qed" tokens''
    return (tokens''', body)

parse_proof_body :: [Token] -> Either String ([Token], [Stmt])
parse_proof_body tokens = case (parse_stmt tokens) of
                            Left l -> Left l
                            Right (Symbol "qed":tokens', stmt) -> Right (Symbol "qed":tokens', [stmt])
                            Right (tokens', stmt) -> case (parse_proof_body tokens') of
                                Left l -> Left l
                                Right (tokens'', stmts) -> Right (tokens'', (stmt:stmts))

parse_stmt :: [Token] -> Either String ([Token], Stmt)
parse_stmt (Symbol "print":rest) = Right (rest, Print)
parse_stmt (Symbol "axiom":Symbol s:rest) = Right (rest, Axiom s)
parse_stmt (Symbol "expect":tokens) = do
    (tokens', seq) <- Sequent.parse_prefix tokens
    return (tokens', Expect seq)
parse_stmt [] = Left "no statement found"
parse_stmt rem = Left $ "could not make sense of: " ++ (show rem)
