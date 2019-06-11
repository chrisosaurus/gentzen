module System.Check
(
    check_rule,
    check_system,
    check,
)
where

import Sequent.Data.Sequent
import System.Data.Rewrite
import System.Data.Rule
import System.Data.System

check :: System -> Either String ()
check system = check_system system

-- check that a given system is well formed
--
-- check to do:
--  every rule name is unique
--  every rule is well formed
check_system :: System -> Either String ()
check_system System { system_name = system_name
                    , rules = rules
                    } = do
    ctx <- return $ "in system '" ++ system_name ++ "': "
    () <- check_rule_names_unique ctx rules
    () <- check_rules ctx rules
    return ()

check_rule_names_unique :: String -> [Rule] -> Either String ()
check_rule_names_unique ctx rules = do
    ctx <- return $ ctx ++ "rule "
    rule_names <- return $ extract_rule_names rules
    () <- all_unique ctx rule_names
    return ()

extract_rule_names :: [Rule] -> [String]
extract_rule_names [] = []
extract_rule_names (Rule { rule_name = rule_name}:xs) = rule_name:(extract_rule_names xs)

check_rules :: String -> [Rule] -> Either String ()
check_rules ctx [] = Right ()
check_rules ctx (x:xs) = do
    () <- check_rule ctx x
    () <- check_rules ctx xs
    return ()

-- check that a given rule is well formed
--
-- checks to do:
--  every symbol in args is unique
--  left_name and right_name are unique and not in args
--  every symbol mentioned in props is mentioned in args, left_name, or right_name
--  every symbol mentioned in body is mentioned in args, left_name, or right_name
check_rule :: String -> Rule -> Either String ()
check_rule ctx Rule{ rule_name = rule_name
                   , args = args
                   , left_name = left_name
                   , right_name = right_name
                   , props = props
                   , body = body
                   } = do
    arg_symbols <- return $ extract_sequent_list_symbols args
    env_symbols <- return $ (left_name:right_name:arg_symbols)
    ctx' <- return $ ctx ++ "in rule '" ++ rule_name ++ "' arguments: "
    () <- all_unique ctx' env_symbols

    props_symbols <- return $ extract_props_symbols props
    ctx' <- return $ ctx ++ "in rule '" ++ rule_name ++ "' propositions: "
    () <- check_defined ctx' props_symbols env_symbols

    body_symbols <- return $ extract_body_symbols body
    ctx' <- return $ ctx ++ "in rule '" ++ rule_name ++ "' rewrite rule: "
    () <- check_defined ctx' body_symbols env_symbols
    return ()

-- check that every string is unique (appears only once)
all_unique :: String -> [String] -> Either String ()
all_unique _   []                   = Right ()
all_unique ctx (x:xs) | x `elem` xs = Left $ ctx ++ "'" ++ x ++ "' is repeated."
all_unique ctx (x:xs)               = all_unique ctx xs

-- check that every symbol in left list is defined in right list
check_defined :: String -> [String] -> [String] -> Either String ()
check_defined _   []     _                = Right ()
check_defined ctx (x:xs) ys | x `elem` ys = check_defined ctx xs ys
check_defined ctx (x:xs) ys               = Left $ ctx ++ "'" ++ x ++ "' used but not defined."


extract_sequent_list_symbols :: [Exp] -> [String]
extract_sequent_list_symbols exps = concat $ map extract_sequent_symbols exps

extract_sequent_symbols :: Exp -> [String]
extract_sequent_symbols Bottom          = []
extract_sequent_symbols (Symbol s)      = [s]
extract_sequent_symbols (And l r)       = (extract_sequent_symbols l) ++ (extract_sequent_symbols r)
extract_sequent_symbols (Or l r)        = (extract_sequent_symbols l) ++ (extract_sequent_symbols r)
extract_sequent_symbols (Implies l r)   = (extract_sequent_symbols l) ++ (extract_sequent_symbols r)
-- TODO how should we handle forall, exists, and subst?
extract_sequent_symbols (Forall  q e)   = [q] ++ (extract_sequent_symbols e)
extract_sequent_symbols (Exists  q e)   = [q] ++ (extract_sequent_symbols e)
extract_sequent_symbols (Subst   a b c) = (extract_sequent_symbols a) ++ (extract_sequent_symbols b) ++ [c]

extract_props_symbols :: [Prop] -> [String]
extract_props_symbols [] = []
extract_props_symbols (x:xs) = (extract_prop_symbols x) ++ (extract_props_symbols xs)

extract_prop_symbols :: Prop -> [String]
extract_prop_symbols (In l r)         = (extract_sequent_symbols l) ++ [r]
extract_prop_symbols (FreeIn l r)     = (extract_sequent_symbols l) ++ [r]
extract_prop_symbols (NotFreeIn l r)  = (extract_sequent_symbols l) ++ [r]
-- TODO tidy these up...
extract_prop_symbols (InSet l set)    = foldl (++) [] $ (extract_sequent_symbols l):(map extract_sequent_symbols set)
extract_prop_symbols (NotInSet l set) = foldl (++) [] $ (extract_sequent_symbols l):(map extract_sequent_symbols set)

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




