module System.Check
(
    check,
)
where

import Sequent.Data.Sequent
import System.Data.Rewrite
import System.Data.Rule

-- check that a given rule is well formed
check :: Rule -> Either String ()
check Rule{ rule_name = ruleName
          , args = args
          , left_name = left_name
          , right_name = right_name
          , props = props
          , body = body
          } = Left "System.Check.check unimplemented"

extract_sequent_symbols :: Exp -> [String]
extract_sequent_symbols Bottom        = []
extract_sequent_symbols (Symbol s)    = [s]
extract_sequent_symbols (Bracketed e) = extract_sequent_symbols e
extract_sequent_symbols (And l r)     = (extract_sequent_symbols l) ++ (extract_sequent_symbols r)
extract_sequent_symbols (Or l r)      = (extract_sequent_symbols l) ++ (extract_sequent_symbols r)
extract_sequent_symbols (Implies l r) = (extract_sequent_symbols l) ++ (extract_sequent_symbols r)

extract_props_symbols :: [Prop] -> [String]
extract_props_symbols [] = []
extract_props_symbols (x:xs) = (extract_prop_symbols x) ++ (extract_props_symbols xs)

extract_prop_symbols :: Prop -> [String]
extract_prop_symbols (In l r) = [l, r]

extract_body_symbols :: Body -> [String]
extract_body_symbols Unit = []
extract_body_symbols (Single sequ) = extract_sequ_symbols sequ
extract_body_symbols (Pair l r) = (extract_sequ_symbols l) ++ (extract_sequ_symbols r)

extract_sequ_symbols :: Sequ -> [String]
extract_sequ_symbols (Sequ l r) = (extract_term_symbols l) ++ (extract_term_symbols r)

extract_term_symbols :: Term -> [String]
extract_term_symbols Empty = []
extract_term_symbols (SExp sexp) = extract_sequent_symbols sexp
extract_term_symbols (Expr n exprs) = n:(extract_exprs_symbols exprs)

extract_exprs_symbols :: [Expr] -> [String]
extract_exprs_symbols [] = []
extract_exprs_symbols (x:xs) = (extract_expr_symbols x) ++ (extract_exprs_symbols xs)

extract_expr_symbols :: Expr -> [String]
extract_expr_symbols (Add sexp) = extract_sequent_symbols sexp
extract_expr_symbols (Remove sexp) = extract_sequent_symbols sexp



-- checks to do:
--  every symbol in args is unique
--  left_name and right_name are unique and not in args
--  every symbol mentioned in props is mentioned in args, left_name, or right_name
--  every symbol mentioned in body is mentioned in args, left_name, or right_name

