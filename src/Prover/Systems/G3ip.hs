module Prover.Systems.G3ip
(
    axiom,
    left_and,
    right_and,
    left_or,
    right_or1,
    right_or2,
    left_implies,
    right_implies,
    left_bottom,
    system_g3ip,
)
where

import Sequent.Data.Sequent
import Prover.Data.Rule
import Prover.Data.System

axiom :: Rule
axiom = Axiom "axiom" axiom'

axiom' :: [Exp] -> Sequent -> Either String ()
axiom' [exp] sequent = do
    sequent'  <- remove_exp_lhs exp sequent
    sequent'' <- remove_exp_rhs exp sequent'
    return ()
axiom' args _ = Left $ "wrong number of args for axiom': " ++ show args


left_and :: Rule
left_and = Unary "left-and" left_and'

left_and' :: [Exp] -> Sequent -> Either String Sequent
left_and' [And l r] sequent = do
    sequent'  <- remove_exp_lhs (And l r) sequent
    let sequent'' = add_exps_lhs [l, r] sequent'
    return sequent''
left_and' args _ = Left $ "wrong args for left_and': " ++ show args


right_and :: Rule
right_and = Binary "right-and" right_and'

right_and' :: [Exp] -> Sequent -> Either String (Sequent, Sequent)
right_and' [And l r] sequent = do
    sequent'   <- remove_exp_rhs (And l r) sequent
    let sequent_l  = add_exp_rhs l sequent'
    let sequent_r  = add_exp_rhs r sequent'
    return (sequent_l, sequent_r)
right_and' args _ = Left $ "wrong args for right_and': " ++ show args


left_or :: Rule
left_or = Binary "left-or" left_or'

left_or' :: [Exp] -> Sequent -> Either String (Sequent, Sequent)
left_or' [Or l r] sequent = do
    sequent'   <- remove_exp_lhs (Or l r) sequent
    let sequent_l  = add_exp_lhs l sequent'
    let sequent_r  = add_exp_lhs r sequent'
    return (sequent_l, sequent_r)
left_or' args _ = Left $ "wrong args for left_or': " ++ show args


right_or1 :: Rule
right_or1 = Unary "right-or1" right_or1'

right_or1' :: [Exp] -> Sequent -> Either String Sequent
right_or1' [Or l r] sequent = do
    sequent'  <- remove_exp_rhs (Or l r) sequent
    let sequent'' = add_exp_rhs l sequent'
    return sequent''
right_or1' args _ = Left $ "wrong args for right_or1': " ++ show args


right_or2 :: Rule
right_or2 = Unary "right-or2" right_or2'

right_or2' :: [Exp] -> Sequent -> Either String Sequent
right_or2' [Or l r] sequent = do
    sequent'  <- remove_exp_rhs (Or l r) sequent
    let sequent'' = add_exp_rhs r sequent'
    return sequent''
right_or2' args _ = Left $ "wrong args for right_or2': " ++ show args


left_implies :: Rule
left_implies = Binary "left-implies" left_implies'

left_implies' :: [Exp] -> Sequent -> Either String (Sequent, Sequent)
left_implies' [Implies l r] sequent@(Sequent lhs rhs) = do
    let sequent_l  = Sequent lhs [l]
    sequent_r  <- remove_exp_lhs (Implies l r) sequent
    let sequent_r' = add_exp_lhs r sequent_r
    return (sequent_l, sequent_r')
left_implies' args _ = Left $ "wrong args for left_implies': " ++ show args


right_implies :: Rule
right_implies = Unary "right-implies" right_implies'

right_implies' :: [Exp] -> Sequent -> Either String Sequent
right_implies' [Implies l r] sequent = do
    sequent'   <- remove_exp_rhs (Implies l r) sequent
    let sequent''  = add_exp_lhs l sequent'
    let sequent''' = add_exp_rhs r sequent''
    return sequent'''
right_implies' args _ = Left $ "wrong args for right_implies': " ++ show args


left_bottom :: Rule
left_bottom = Axiom "left-bottom" left_bottom'

left_bottom' :: [Exp] -> Sequent -> Either String ()
left_bottom' [] sequent = do
    sequent' <- remove_exp_lhs Bottom sequent
    return ()
left_bottom' args _ = Left $ "wrong args for left_bottom': " ++ show args


system_g3ip :: System
system_g3ip = System "g3ip" [ axiom
                            , left_and
                            , right_and
                            , left_or
                            , right_or1
                            , right_or2
                            , left_implies
                            , right_implies
                            , left_bottom
                            ]

