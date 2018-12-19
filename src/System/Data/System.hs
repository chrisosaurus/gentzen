module System.Data.System
(
    System (..),
    lookup_rule,
    lookup_system,
)
where

import Sequent.Data.Sequent
import System.Data.Rewrite

data System = System { system_name  :: String
                     , rules :: [Rule]
                     }
    deriving (Eq)

instance Show System where
    show (System name _) = "system '" ++ name ++ "'"

lookup_rule :: System -> String -> Either String Rule
lookup_rule System { system_name=system_name
                   , rules=rules
                   } rule_name = lookup_rule' system_name rules rule_name

lookup_rule' :: String -> [Rule] -> String -> Either String Rule
lookup_rule' system [] rule = Left $
    "Lookup failed for rule '" ++ rule ++ "' in system '" ++ system ++ "'"
lookup_rule' system (x:_)  rule | match x rule = Right x
lookup_rule' system (_:xs) rule                = lookup_rule' system xs rule

match :: Rule -> String -> Bool
match Rule {rule_name=rule} rule' | rule == rule' = True
match _                     _                     = False

lookup_system :: [System] -> String -> Either String System
lookup_system [] name = Left $ "System '" ++ name ++ "' was not found."
lookup_system (x:_)  name | match_system x name = Right x
lookup_system (_:xs) name                       = lookup_system xs name

match_system :: System -> String -> Bool
match_system System {system_name=name} name' | name == name' = True
match_system _                         _                     = False

