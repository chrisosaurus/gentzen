module System.Data.Rule
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

remove_exp :: Exp -> [Exp] -> Either String [Exp]
remove_exp needle (head:tail) | needle == head = Right tail
remove_exp needle (head:tail) = case (remove_exp needle tail) of
                                Left  l -> Left l
                                Right r -> Right (head:r)
remove_exp needle []          = Left $ "Failed to find needle: " ++ (show needle)

--               needle haystack                  haystack-needle
remove_exp_lhs :: Exp -> Sequent -> Either String (Sequent)
remove_exp_lhs exp (Sequent lhs rhs) = case remove_exp exp lhs of
                                        Left  l    -> Left l
                                        Right lhs' -> Right (Sequent lhs' rhs)
--                needle haystack                 haystack-needle
remove_exp_rhs :: Exp -> Sequent -> Either String Sequent
remove_exp_rhs exp (Sequent lhs rhs) = case remove_exp exp rhs of
                                        Left  l    -> Left l
                                        Right rhs' -> Right (Sequent lhs  rhs')

add_exp :: Exp -> [Exp] -> [Exp]
add_exp exp lst = (exp:lst)

add_exps :: [Exp] -> [Exp] -> [Exp]
add_exps exp lst = (exp ++ lst)

add_exp_lhs :: Exp -> Sequent -> Sequent
add_exp_lhs exp (Sequent lhs rhs) = (Sequent (exp:lhs) rhs)

add_exps_lhs :: [Exp] -> Sequent -> Sequent
add_exps_lhs exps (Sequent lhs rhs) = (Sequent (exps ++ lhs) rhs)

add_exp_rhs :: Exp -> Sequent -> Sequent
add_exp_rhs exp (Sequent lhs rhs) = (Sequent lhs (exp:rhs))

add_exps_rhs :: [Exp] -> Sequent -> Sequent
add_exps_rhs exps (Sequent lhs rhs) = (Sequent lhs (exps ++ rhs))

split_exp :: Exp -> Either String (Exp, Exp)
split_exp (And     l r) = Right (l, r)
split_exp (Or      l r) = Right (l, r)
split_exp (Implies l r) = Right (l, r)
split_exp (Bracketed e) = Left "Cannot split Implies"
split_exp (Symbol s)    = Left "Cannot split Symbol"

split_exp_lhs :: Exp -> Either String Exp
split_exp_lhs exp = case (split_exp exp) of
                                Left l         -> Left l
                                Right (lhs, _) -> Right lhs

split_exp_rhs :: Exp -> Either String Exp
split_exp_rhs exp = case (split_exp exp) of
                                Left l         -> Left l
                                Right (_, rhs) -> Right rhs

-- deeply unbracket, removes all outer brackets
unbracket :: Exp -> Exp
unbracket (Bracketed s) = unbracket s
unbracket s = s
