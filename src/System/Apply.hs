module System.Apply
(
    apply,
    destruct_args,
    destruct_args',
    destruct_arg,
    destruct_sequent,
    rewrite,
    rewrite_body,
    rewrite_sequ,
    rewrite_exprs,
    rewrite_expr,
    rewrite_term,
    instantiate,
)
where

import qualified Sequent.Data.Sequent as Sequent
import System.Data.Rewrite
import System.Data.Rule
import qualified System.Data.Env as Env

apply :: Sequent.Sequent -> Rule -> [Sequent.Exp] -> Either String [Sequent.Sequent]
apply from rule@(Rule {rule_name=rule_name}) args = do
    arg_env <- destruct_args rule args
    seq_env <- destruct_sequent from rule
    env     <- Env.merge arg_env seq_env
    result  <- rewrite rule env
    return result

rewrite :: Rule -> Env.Env -> Either String [Sequent.Sequent]
rewrite Rule{body=body} env = rewrite_body body env

rewrite_body :: Body -> Env.Env -> Either String [Sequent.Sequent]
rewrite_body Unit env = Right []
rewrite_body (Single sequ) env = do
    inner <- rewrite_sequ sequ env
    return [inner]
rewrite_body (Pair lsequ rsequ) env = do
    left  <- rewrite_sequ lsequ env
    right <- rewrite_sequ rsequ env
    return [left, right]

rewrite_sequ :: Sequ -> Env.Env -> Either String Sequent.Sequent
rewrite_sequ (Sequ lterm rterm) env = do
    left  <- rewrite_term lterm env
    right <- rewrite_term rterm env
    return $ Sequent.Sequent left right

rewrite_term :: Term -> Env.Env -> Either String [Sequent.Exp]
rewrite_term Empty _ = Right []
rewrite_term (SExp sexp) env = do
    inner <- instantiate sexp env
    return [inner]
rewrite_term (Expr str exprs) env = do
    start <- Env.get env str
    result <- rewrite_exprs exprs env start
    return result

rewrite_exprs :: [Expr] -> Env.Env -> [Sequent.Exp] -> Either String [Sequent.Exp]
rewrite_exprs []     _   state = Right state
rewrite_exprs (x:xs) env state = do
    state <- rewrite_expr x env state
    state <- rewrite_exprs xs env state
    return state

rewrite_expr :: Expr -> Env.Env -> [Sequent.Exp] -> Either String [Sequent.Exp]
rewrite_expr (Add    sexp) env state = do
    sexp  <- instantiate sexp env
    state <- return $ add_exp sexp state
    return state
rewrite_expr (Remove sexp) env state = do
    sexp  <- instantiate sexp env
    state <- remove_exp sexp state
    return state

instantiate :: Sequent.Exp -> Env.Env -> Either String Sequent.Exp
instantiate (Sequent.And l r) env = do
    l <- instantiate l env
    r <- instantiate r env
    return $ Sequent.And l r
instantiate (Sequent.Or l r) env = do
    l <- instantiate l env
    r <- instantiate r env
    return $ Sequent.Or l r
instantiate (Sequent.Implies l r) env = do
    l <- instantiate l env
    r <- instantiate r env
    return $ Sequent.Implies l r
instantiate (Sequent.Bracketed i) env = do
    i <- instantiate i env
    return $ Sequent.Bracketed i
instantiate Sequent.Bottom _ = Right Sequent.Bottom
instantiate (Sequent.Symbol s) env = do
    val <- Env.get env s
    val <- expect_single val
    return val

expect_single :: [Sequent.Exp] -> Either String Sequent.Exp
expect_single []     = Left "System.Apply.expect_single: empty found"
expect_single (x:[]) = Right x
expect_single _      = Left "System.Apply.expect_single: multiple found"

destruct_args :: Rule -> [Sequent.Exp] -> Either String Env.Env
destruct_args Rule {args=[]} [] = Right Env.EnvEmpty
destruct_args Rule { rule_name=rule_name
                   , args=bindings} args | (length bindings) /= (length args) =
    Left $ "Argument number mismatch for rule '" ++ rule_name ++ "', expected: " ++ (show (length bindings)) ++ ", but got " ++ (show (length args)) ++"."
destruct_args Rule { rule_name = rule_name
                  , args = bindings } args = destruct_args' bindings args

destruct_args' :: [Sequent.Exp] -> [Sequent.Exp] -> Either String Env.Env
destruct_args' (x:xs) (y:ys) = do
    left  <- destruct_arg x y
    right <- destruct_args' xs ys
    env   <- Env.merge left right
    return env
destruct_args' [] [] = Right Env.EnvEmpty
destruct_args' _  [] = Left "Too few arguments."
destruct_args' [] _  = Left "Too many arguments."

destruct_arg :: Sequent.Exp -> Sequent.Exp -> Either String Env.Env
destruct_arg (Sequent.And bl br) (Sequent.And al ar) = do
    left  <- destruct_arg bl al
    right <- destruct_arg br ar
    env   <- Env.merge left right
    return env
destruct_arg (Sequent.Or bl br) (Sequent.Or al ar) = do
    left  <- destruct_arg bl al
    right <- destruct_arg br ar
    env   <-  Env.merge left right
    return env
destruct_arg (Sequent.Implies bl br) (Sequent.Implies al ar) = do
    left  <- destruct_arg bl al
    right <- destruct_arg br ar
    env   <-  Env.merge left right
    return env
destruct_arg (Sequent.Bracketed b) (Sequent.Bracketed a) = do
    destruct_arg b a
destruct_arg (Sequent.Symbol name) arg = do
    return $ Env.Env name [arg] Env.EnvEmpty
destruct_arg Sequent.Bottom _ = Left "Apply error: bottom encountered as binding"
destruct_arg binding argument = Left $ "Apply error: binding '" ++ (show binding) ++ "' doesn't match argument '" ++ (show argument) ++ "'."

destruct_sequent :: Sequent.Sequent -> Rule -> Either String Env.Env
destruct_sequent (Sequent.Sequent l r) Rule { left_name  = left_name
                                            , right_name = right_name } = do
    return $ Env.Env left_name l $ Env.Env right_name r $ Env.EnvEmpty

