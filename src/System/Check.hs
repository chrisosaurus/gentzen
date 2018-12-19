module System.Check
(
    check_rule,
)
where

import Sequent.Data.Sequent
import System.Data.Rewrite
import System.Data.Rule

-- checks to do:
--  every symbol in args is unique
--  left_name and right_name are unique and not in args
--  every symbol mentioned in props is mentioned in args, left_name, or right_name
--  every symbol mentioned in body is mentioned in args, left_name, or right_name

-- check that a given rule is well formed
check_rule :: Rule -> Either String ()
check_rule Rule{ rule_name = rule_name
               , args = args
               , left_name = left_name
               , right_name = right_name
               , props = props
               , body = body
               } = do
    arg_symbols <- return $ extract_sequent_list_symbols args
    env_symbols <- return $ (left_name:right_name:arg_symbols)
    () <- all_unique rule_name env_symbols
    props_symbols <- return $ extract_props_symbols props
    () <- check_defined rule_name props_symbols env_symbols
    body_symbols <- return $ extract_body_symbols body
    () <- check_defined rule_name body_symbols env_symbols
    return ()

-- check that every string is unique (appears only once)
all_unique :: String -> [String] -> Either String ()
all_unique _         []                   = Right ()
all_unique rule_name (x:xs) | x `elem` xs = Left $ "in rule '" ++ rule_name ++ "': argument '" ++ x ++ "' is repeated."
all_unique rule_name (x:xs)               = all_unique rule_name xs

-- check that every symbol in left list is defined in right list
check_defined :: String -> [String] -> [String] -> Either String ()
check_defined _                            [] _ = Right ()
check_defined rule_name (x:xs) ys | x `elem` ys = check_defined rule_name xs ys
check_defined rule_name (x:xs) ys               = Left $ "in rule '" ++ rule_name ++ "': symbol '" ++ x ++ "' used in rewrite rule but not defined."


extract_sequent_list_symbols :: [Exp] -> [String]
extract_sequent_list_symbols exps = concat $ map extract_sequent_symbols exps

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




