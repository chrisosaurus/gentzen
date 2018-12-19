module System.Apply
(
    apply,
    destruct_args,
    destruct_args',
    destruct_arg,
    destruct_sequent,
)
where

import qualified Sequent.Data.Sequent as Sequent
import System.Data.Rewrite
import System.Data.Rule
import qualified System.Data.Env as Env

apply :: Sequent.Sequent -> Rule -> [Sequent.Exp] -> Either String Sequent.Sequent
apply _ _ _  = Left "System.Apply.apply unimplemented"

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

