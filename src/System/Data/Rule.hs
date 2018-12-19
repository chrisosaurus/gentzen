module System.Data.Rule
(
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
