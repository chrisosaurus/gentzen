module Systems.Data.Rule
(
    Rule (..),
    apply_axiom,
    apply_unary,
    apply_binary,
    remove_exp,
    remove_exp_lhs,
    remove_exp_rhs,
    add_exp,
    add_exps,
    add_exp_lhs,
    add_exps_lhs,
    add_exp_rhs,
    add_exps_rhs,
    split_exp,
    split_exp_lhs,
    split_exp_rhs,
    unbracket,
)
where

import Sequent.Data.Sequent
import System.Data.Rule

--                 name    args     input                    output
data Rule = Axiom  String ([Exp] -> Sequent -> Either String ())
          | Unary  String ([Exp] -> Sequent -> Either String Sequent)
          | Binary String ([Exp] -> Sequent -> Either String (Sequent, Sequent))

instance Show Rule where
    show (Axiom  name _) = "rule " ++ name
    show (Unary  name _) = "rule " ++ name
    show (Binary name _) = "rule " ++ name

instance Eq Rule where
    (==) (Axiom  name _) (Axiom  name' _) = name == name'
    (==) (Unary  name _) (Unary  name' _) = name == name'
    (==) (Binary name _) (Binary name' _) = name == name'
    (==) _ _ = False

apply_axiom :: Rule -> [Exp] -> Sequent -> Either String ()
apply_axiom (Axiom _ func) args sequent = func args sequent
apply_axiom (Unary  name _) _ _ = Left $ "Rule '" ++ name ++ "' was Unary not an Axiom"
apply_axiom (Binary name _) _ _ = Left $ "Rule '" ++ name ++ "' was Binary not an Axiom"

apply_unary :: Rule -> [Exp] -> Sequent -> Either String Sequent
apply_unary (Unary _ func) args sequent = func args sequent
apply_unary (Axiom name _) _ _ = Left $ "Rule '" ++ name ++ "' was an Axiom not an Unary"
apply_unary (Binary name _) _ _ = Left $ "Rule '" ++ name ++ "' was Binary not an Unary"

apply_binary :: Rule -> [Exp] -> Sequent -> Either String (Sequent, Sequent)
apply_binary (Binary _ func) args sequent = func args sequent
apply_binary (Axiom name _) _ _ = Left $ "Rule '" ++ name ++ "' was an Axiom not binary"
apply_binary (Unary name _) _ _ = Left $ "Rule '" ++ name ++ "' was Unary not binary"

