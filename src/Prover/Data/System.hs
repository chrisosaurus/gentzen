module Prover.Data.System
(
    System (..),
    find_rule,
)
where

import Sequent.Data.Sequent
import Prover.Data.Rule

--                   name   rules
data System = System String [Rule]

instance Show System where
    show (System name _) = "system " ++ name

find_rule :: System -> String -> Either String Rule
find_rule (System system_name rules) rule_name = case (find_rule' rules rule_name) of
                                                    Nothing -> Left $ "Could not find rule '" ++ rule_name ++ "' in system '" ++ system_name ++ "'"
                                                    Just rule -> Right rule

find_rule' :: [Rule] -> String -> Maybe Rule
find_rule' [] _ = Nothing
find_rule' (rule@(Unary  name _):tail) name' | name == name' = Just rule
find_rule' (rule@(Binary name _):tail) name' | name == name' = Just rule
find_rule' (x:xs) name = find_rule' xs name
