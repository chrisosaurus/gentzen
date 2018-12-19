module Proof.Parser
(
    parse,
)
where

import Data.Token
import qualified Sequent.Data.Sequent as Sequent
import qualified Sequent.Parser as Sequent
import Proof.Data.Theorem
import ParserUtils
import Data.Semigroup ((<>))

parse :: [Token] -> Either String Theorem
parse tokens = do
    (tokens, name) <- parse_name tokens
    (tokens, system) <- parse_system tokens
    (tokens, sequent) <- parse_sequent tokens
    (tokens, steps) <- parse_proof tokens
    [] <- expect_empty tokens
    return Theorem {name=name, system=system, sequent=sequent, steps=steps}

parse_name :: [Token] -> Either String ([Token], String)
parse_name tokens = do
    tokens <- consume_symbol "theorem" tokens
    (tokens, str) <- parse_string tokens
    return (tokens, str)

parse_system :: [Token] -> Either String ([Token], String)
parse_system tokens = do
    tokens <- consume_symbol "system" tokens
    (tokens, str) <- parse_string tokens
    return (tokens, str)

parse_sequent :: [Token] -> Either String ([Token], Sequent.Sequent)
parse_sequent tokens = do
    tokens <- consume_symbol "sequent" tokens
    (tokens, sequent) <- Sequent.parse_prefix tokens
    return (tokens, sequent)

parse_proof :: [Token] -> Either String ([Token], [Stmt])
parse_proof tokens = do
    tokens <- consume_symbol "proof" tokens
    (tokens, body) <- parse_proof_body tokens
    tokens <- consume_symbol "qed" tokens
    return (tokens, body)

parse_proof_body :: [Token] -> Either String ([Token], [Stmt])
parse_proof_body tokens = do
    (tokens, stmt) <- parse_stmt tokens
    (tokens, stmts) <- (qed tokens) <> (rcurly tokens) <> (more tokens)
    return $ (tokens, (stmt:stmts))
    where
        -- stop if we find 'qed'
        qed :: [Token] -> Either String ([Token], [Stmt])
        qed tokens = do
            tokens <- expect_token (Symbol "qed") tokens
            return (tokens, [])
        -- stop if we find '}'
        rcurly :: [Token] -> Either String ([Token], [Stmt])
        rcurly tokens = do
            tokens <- expect_token (RCurly) tokens
            return (tokens, [])
        -- otherwise keep going
        more :: [Token] -> Either String ([Token], [Stmt])
        more tokens = do
            (tokens, stmts) <- parse_proof_body tokens
            return (tokens, stmts)

parse_stmt :: [Token] -> Either String ([Token], Stmt)
parse_stmt (LCurly:tokens) = do
    (tokens, left) <- parse_proof_body tokens
    tokens <- consume_token RCurly tokens
    tokens <- consume_token LCurly tokens
    (tokens, right) <- parse_proof_body tokens
    tokens <- consume_token RCurly tokens
    return (tokens, Branch left right)
parse_stmt (Symbol "print":rest) = Right (rest, Print)
parse_stmt (Symbol "abort":rest) = Right (rest, Abort)
parse_stmt (Symbol "expect":tokens) = do
    (tokens, seq) <- Sequent.parse_prefix tokens
    return (tokens, Expect seq)
parse_stmt (Symbol "apply":Symbol name:LSquare:RSquare:tokens) = do
    return (tokens, Apply name [])
parse_stmt (Symbol "apply":Symbol name:LSquare:tokens) = do
    (tokens, arguments) <- Sequent.parse_commalist tokens
    tokens <- consume_token RSquare tokens
    return (tokens, Apply name arguments)
parse_stmt [] = Left "no statement found"
parse_stmt rem = Left $ "could not make sense of: " ++ (show rem)

