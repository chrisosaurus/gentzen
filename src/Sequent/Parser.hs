module Sequent.Parser
(
    parse,
    parse_prefix,
    parse_exp,
    parse_commalist,
)
where

import qualified Sequent.Data.Sequent as Sequent
import Data.Token
import Control.Applicative
import ParserUtils

-- parse consumes all input and raises error if there is any left
parse :: [Token] -> Either String Sequent.Sequent
parse [] = Left "No tokens, expected Sequent"
parse toks = do
    (toks, seq) <- parse_prefix toks
    [] <- expect_empty toks
    return seq

-- parse_prefix will greedily consume a sequent from the front, but leave the
-- remaining tokens untouched
parse_prefix :: [Token] -> Either String ([Token], Sequent.Sequent)
parse_prefix [] = Left "No tokens, expected Sequent"
parse_prefix toks = do
    (toks, lhs) <- parse_commalist toks
    toks <- consume_token Turnstyle toks
    (toks, rhs) <- parse_commalist toks
    let seq = Sequent.Sequent lhs rhs
    return (toks, seq)

parse_commalist :: [Token] -> Either String ([Token], [Sequent.Exp])
parse_commalist [] = Right ([], [])
parse_commalist all@(Turnstyle:_) = Right (all, [])
parse_commalist toks = case (parse_exp toks) of
                            Left l -> Left l
                            Right (Comma:toks', exp) -> case (parse_commalist toks') of
                                                            Left l -> Left l
                                                            Right (toks'', exp') -> Right (toks'', (exp:exp'))
                            Right (toks, exp) -> Right (toks, [exp])

parse_exp :: [Token] -> Either String ([Token], Sequent.Exp)
parse_exp (LParen:toks) = do
    (toks, inner) <- parse_exp toks
    toks <- consume_token RParen toks
    return (toks, Sequent.Bracketed inner)
parse_exp toks = do
    (toks, single) <- parse_single toks
    return (toks, single)

parse_single :: [Token] -> Either String ([Token], Sequent.Exp)
parse_single (Bottom:rest) = do
    return (rest, Sequent.Bottom)
parse_single (Symbol l:Implies:rest) = do
    (rest, rhs) <- parse_exp rest
    return (rest, Sequent.Implies (Sequent.Symbol l) rhs)
parse_single (Symbol l:And:rest) = do
    (rest, rhs) <- parse_exp rest
    return (rest, Sequent.And (Sequent.Symbol l) rhs)
parse_single (Symbol l:Or:rest) = do
    (rest, rhs) <- parse_exp rest
    return (rest, Sequent.Or (Sequent.Symbol l) rhs)
parse_single (Symbol s:rest) = Right $ (rest, Sequent.Symbol s)
parse_single [] = Left "parse_single: empty list found"
parse_single _  = Left "parse_single: unknown error"

