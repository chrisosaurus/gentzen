module System.Parser
(
    parse,
    parse_prefix,
    parse_rule,
    parse_props,
    parse_prop,
    parse_term,
    parse_exprs,
    parse_sequ,
)
where

import ParserUtils
import Data.Token
import System.Data.System
import System.Data.Rewrite
import Data.Semigroup ((<>))
import qualified Sequent.Data.Sequent as Sequent
import qualified Sequent.Parser as Sequent

-- parse consumes all input and raises error if there is any left
parse :: [Token] -> Either String System
parse tokens = do
    (tokens, system) <- parse_prefix tokens
    [] <- expect_empty tokens
    return system

-- parse_prefix will greedily consume a system from the front, but leave the
-- remaining tokens untouched
parse_prefix :: [Token] -> Either String ([Token], System)
parse_prefix tokens = do
    (tokens, name) <- parse_name tokens
    (tokens, rules) <- parse_rules tokens
    return (tokens, System name rules)

parse_name :: [Token] -> Either String ([Token], String)
parse_name tokens = do
    tokens <- consume_symbol "system" tokens
    (tokens, str) <- parse_string tokens
    return (tokens, str)

parse_rules :: [Token] -> Either String ([Token], [Rule])
parse_rules tokens = do
    tokens <- consume_symbol "rules" tokens
    (tokens, body) <- parse_rules_body tokens
    tokens <- consume_symbol "qed" tokens
    return (tokens, body)

parse_rules_body :: [Token] -> Either String ([Token], [Rule])
parse_rules_body tokens = do
    (tokens, rule) <- parse_rule tokens
    (tokens, rules) <- (qed tokens) <> (more tokens)
    return (tokens, (rule:rules))
    where
        -- stop if we find 'qed'
        qed :: [Token] -> Either String ([Token], [Rule])
        qed tokens = do
            tokens <- expect_token (Symbol "qed") tokens
            return (tokens, [])
        -- otherwise keep going
        more :: [Token] -> Either String ([Token], [Rule])
        more tokens = do
            (tokens, rules) <- parse_rules_body tokens
            return (tokens, rules)

parse_rule :: [Token] -> Either String ([Token], Rule)
parse_rule tokens = do
    (tokens, name) <- parse_string tokens
    (tokens, args) <- parse_args tokens
    (tokens, lname, rname) <- parse_sequent_arg tokens
    tokens <- consume_token Equal tokens
    (tokens, props) <- parse_props tokens
    (tokens, body) <- parse_body tokens
    return (tokens, Rule { rule_name = name
                         , args = args
                         , left_name = lname
                         , right_name = rname
                         , props = props
                         , body = body
                         })

parse_args' :: [Token] -> Either String ([Token], [Sequent.Exp])
-- parse_args' must consume at least one argument
-- parse_args' is only called if either:
--  we have consumed LParen and the next token is NOT RParen
--  we are inside rule arguments and have just consumed a Comma
parse_args' (RParen:_) = Left "expected rewrite rule argument"
parse_args' tokens = do
    (tokens, arg)  <- Sequent.parse_exp tokens
    (tokens, args) <- (more tokens) <> (stop tokens)
    return (tokens, arg:args)
    where
        -- stop if we find a closing left paren ')'
        stop :: [Token] -> Either String ([Token], [Sequent.Exp])
        stop tokens = do
            tokens <- expect_token RParen tokens
            return (tokens, [])
        -- keep going if we find a comma ','
        more :: [Token] -> Either String ([Token], [Sequent.Exp])
        more tokens = do
            tokens <- consume_token Comma tokens
            (tokens, args) <- parse_args' tokens
            return (tokens, args)

parse_args :: [Token] -> Either String ([Token], [Sequent.Exp])
parse_args (LParen:RParen:tokens) = Right (tokens, [])
parse_args tokens = do
    tokens <- consume_token LParen tokens
    (tokens, args) <- parse_args' tokens
    tokens <- consume_token RParen tokens
    return (tokens, args)

parse_sequent_arg :: [Token] -> Either String ([Token], String, String)
parse_sequent_arg tokens = do
    tokens <- consume_token LParen tokens
    (tokens, lname) <- parse_string tokens
    tokens <- consume_token Turnstile tokens
    (tokens, rname) <- parse_string tokens
    tokens <- consume_token RParen tokens
    return (tokens, lname, rname)


parse_props :: [Token] -> Either String ([Token], [Prop])
-- empty props is allowed
parse_props (LSquare:RSquare:tokens) = Right (tokens, [])
parse_props (LSquare:tokens) = do
    (tokens, props) <- parse_props_body tokens
    tokens <- consume_token RSquare tokens
    tokens <- consume_token Period tokens
    return (tokens, props)
-- props are optional, otherwise we have none
parse_props tokens = Right (tokens, [])

parse_props_body :: [Token] -> Either String ([Token], [Prop])
parse_props_body tokens = do
    (tokens, prop) <- parse_prop tokens
    (tokens, props) <- (end tokens) <> (more tokens)
    return (tokens, (prop:props))
    where
        end :: [Token] -> Either String ([Token], [Prop])
        end tokens = do
            tokens <- expect_token RSquare tokens
            return (tokens, [])
        more :: [Token] -> Either String ([Token], [Prop])
        more tokens = do
            tokens <- consume_token Comma tokens
            (tokens, props) <- parse_props_body tokens
            return (tokens, props)

parse_prop :: [Token] -> Either String ([Token], Prop)
parse_prop tokens = do
    tokens <- consume_token LParen tokens
    (tokens, left)   <- Sequent.parse_exp tokens
    (tokens, opname) <- parse_string tokens
    cons             <- case opname of
                        "in"        -> Right In
                        "freein"    -> Right FreeIn
                        "notfreein" -> Right NotFreeIn
                        _           -> Left "failed to parse prop operator"
    (tokens, rname) <- parse_string tokens
    tokens          <- consume_token RParen tokens
    exp             <- return $ cons left rname
    return (tokens, exp)

parse_body :: [Token] -> Either String ([Token], Body)
-- unit
parse_body (LParen:RParen:tokens) = Right (tokens, Unit)
-- double
parse_body (LParen:tokens) = do
    (tokens, left) <- parse_sequ tokens
    tokens <- consume_token RParen tokens
    tokens <- consume_token Comma tokens
    tokens <- consume_token LParen tokens
    (tokens, right) <- parse_sequ tokens
    tokens <- consume_token RParen tokens
    return (tokens, Pair left right)
-- single
parse_body tokens = do
    (tokens, sequ) <- parse_sequ tokens
    return (tokens, Single sequ)

parse_sequ :: [Token] -> Either String ([Token], Sequ)
parse_sequ tokens = do
    (tokens, lterm) <- parse_term tokens
    tokens <- consume_token Turnstile tokens
    (tokens, rterm) <- parse_term tokens
    return (tokens, Sequ lterm rterm)

parse_term :: [Token] -> Either String ([Token], Term)
parse_term (LParen:RParen:tokens) = Right (tokens, Empty)
-- Sequent.Exp in brackets
parse_term (LParen:tokens) = do
    (tokens, sexp) <- Sequent.parse_exp tokens
    tokens <- consume_token RParen tokens
    return (tokens, SExp sexp)
-- Expr
parse_term tokens = do
    (tokens, name) <- parse_string tokens
    (tokens, exprs) <- parse_exprs tokens
    return (tokens, Expr name exprs)

parse_exprs :: [Token] -> Either String ([Token], [Expr])
-- debracket sequents
parse_exprs (Minus:LParen:tokens) = do
    (tokens, sexp) <- Sequent.parse_exp tokens
    tokens <- consume_token RParen tokens
    (tokens, sexps) <- parse_exprs tokens
    return (tokens, ((Remove sexp):sexps))
-- otherwise it is just a symbol
parse_exprs (Minus:tokens) = do
    (tokens, name) <- parse_string tokens
    (tokens, sexps) <- parse_exprs tokens
    sexp <- return $ Sequent.Symbol name
    return (tokens, ((Remove sexp):sexps))
-- debracket sequents
parse_exprs (Plus:LParen:tokens) = do
    (tokens, sexp) <- Sequent.parse_exp tokens
    tokens <- consume_token RParen tokens
    (tokens, sexps) <- parse_exprs tokens
    return (tokens, ((Add sexp):sexps))
-- otherwise it is just a symbol
parse_exprs (Plus:tokens) = do
    (tokens, name) <- parse_string tokens
    (tokens, sexps) <- parse_exprs tokens
    sexp <- return $ Sequent.Symbol name
    return (tokens, ((Add sexp):sexps))
parse_exprs tokens = Right (tokens, [])

