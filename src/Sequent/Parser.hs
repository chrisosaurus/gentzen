module Sequent.Parser
(
    parse,
)
where

import qualified Sequent.Data.Sequent as Sequent
import Lexer
import Control.Applicative

parse :: [Token] -> Either String Sequent.Sequent
parse [] = Left "No tokens, expected Sequent"
parse toks = do
    (toks', lhs) <- parse_commalist toks
    toks'' <- remove_token Turnstyle toks'
    (toks''', rhs) <- parse_commalist toks''
    let seq = Sequent.Sequent lhs rhs
    seq' <- expect_empty toks''' seq
    return seq'

expect_empty :: [Token] -> Sequent.Sequent -> Either String Sequent.Sequent
expect_empty [] seq = Right seq
expect_empty rem  _   = Left $ "Failed to parse all of sequent :" ++ (show rem)

remove_token :: Token -> [Token] -> Either String [Token]
remove_token exp (tok:rest) | exp == tok = Right rest
remove_token _ _ = Left "remove_token No token found"

parse_commalist :: [Token] -> Either String ([Token], [Sequent.Exp])
parse_commalist [] = Right ([], [])
parse_commalist all@(Turnstyle:_) = Right (all, [])
parse_commalist toks = case (parse_exp toks) of
                            Left l -> Left l
                            Right (Comma:toks', exp) -> case (parse_commalist toks') of
                                                            Left l -> Left l
                                                            Right (toks'', exp') -> Right (toks'', (exp:exp'))
                            Right (Turnstyle:toks', exp) -> Right (Turnstyle:toks', [exp])
                            Right ([], exp) -> Right ([], [exp])
                            Right (toks', exp) -> Left $ "Unexpected token " ++ show toks'

parse_exp :: [Token] -> Either String ([Token], Sequent.Exp)
parse_exp (LParen:toks) = do
    (toks', inner) <- parse_exp toks
    toks'' <- remove_token RParen toks'
    return (toks'', Sequent.Bracketed inner)
parse_exp toks = do
    (toks', single) <- parse_single toks
    return (toks', single)

parse_single :: [Token] -> Either String ([Token], Sequent.Exp)
parse_single (Symbol l:Implies:rest) = do
    (rest', rhs) <- parse_exp rest
    return (rest', Sequent.Implies (Sequent.Symbol l) rhs)
parse_single (Symbol l:And:rest) = do
    (rest', rhs) <- parse_exp rest
    return (rest', Sequent.And (Sequent.Symbol l) rhs)
parse_single (Symbol l:Or:rest) = do
    (rest', rhs) <- parse_exp rest
    return (rest', Sequent.Or (Sequent.Symbol l) rhs)
parse_single (Symbol s:rest) = Right $ (rest, Sequent.Symbol s)
parse_single [] = Left "parse_single: empty list found"
parse_single _  = Left "parse_single: unknown error"

