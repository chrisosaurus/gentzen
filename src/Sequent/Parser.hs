module Sequent.Parser
(
    parse,
    parse_prefix,
    parse_exp,
    parse_single,
    parse_commalist,
)
where

import qualified Sequent.Data.Sequent as Sequent
import Data.Token
import Control.Applicative
import ParserUtils
import Data.Semigroup ((<>))

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
parse_commalist toks = do
    (toks, exp) <- parse_exp toks
    (toks, exps) <- (more toks) <> (end toks)
    return (toks, (exp:exps))
    where more :: [Token] -> Either String ([Token], [Sequent.Exp])
          more toks = do
            toks <- consume_token Comma toks
            (toks, exps) <- parse_commalist toks
            return (toks, exps)
          end :: [Token] -> Either String ([Token], [Sequent.Exp])
          end toks = Right (toks, [])

parse_exp :: [Token] -> Either String ([Token], Sequent.Exp)
parse_exp tokens = do
    (tokens, exp) <- (parse_quantifiers tokens)  <>
                     (parse_substitution tokens) <>
                     (parse_exp_binary tokens)   <>
                     (parse_single tokens)
    return (tokens, exp)

parse_exp_binary :: [Token] -> Either String ([Token], Sequent.Exp)
parse_exp_binary tokens = do
    (tokens, left)     <- parse_single tokens
    (operator, tokens) <- case tokens of
                        []     -> Left "Failed to find operator"
                        (x:xs) -> Right (x, xs)
    cons               <- translate operator
    (tokens, right)    <- parse_single tokens
    exp                <- return $ cons left right
    return (tokens, exp)

parse_quantifiers :: [Token] -> Either String ([Token], Sequent.Exp)
parse_quantifiers (quantifier:tokens) = do
    cons          <- case quantifier of
                       Forall -> Right Sequent.Forall
                       Exists -> Right Sequent.Exists
                       _      -> Left "no quantifier found"
    (tokens, var) <- parse_string tokens
    (tokens, exp) <- parse_single tokens
    exp           <- return $ cons var exp
    return (tokens, exp)
parse_quantifiers [] = Left "parse_quantifiers: empty list found"

-- A<t/x> or haystack<replacement/needle>
-- search through haystack, replacing every instance of needle with repalcement
parse_substitution :: [Token] -> Either String ([Token], Sequent.Exp)
parse_substitution tokens = do
    (tokens, haystack)    <- parse_single tokens
    tokens                <- consume_token LessThan tokens
    -- TODO for now we allow replacement to be an arbitrary expression
    (tokens, replacement) <- parse_single tokens
    tokens                <- consume_token ForwardSlash tokens
    (tokens, needle)      <- parse_string tokens
    tokens                <- consume_token GreaterThan tokens
    exp                   <- return $ Sequent.Substitute haystack replacement needle
    return (tokens, exp)

parse_single :: [Token] -> Either String ([Token], Sequent.Exp)
parse_single (LParen:rest) = do
    (toks, inner) <- parse_exp rest
    toks <- consume_token RParen toks
    return (toks, inner)
parse_single (Bottom:rest) = do
    return (rest, Sequent.Bottom)
parse_single (Symbol s:rest) = Right $ (rest, Sequent.Symbol s)
parse_single [] = Left "parse_exp: empty list found"
parse_single _  = Left "parse_exp: unknown error"

translate :: Token -> Either String (Sequent.Exp -> Sequent.Exp -> Sequent.Exp)
translate And = Right Sequent.And
translate Or = Right Sequent.Or
translate Implies = Right Sequent.Implies
translate tok = Left $ "Unable to translate operator '" ++ (show tok) ++ "'."

