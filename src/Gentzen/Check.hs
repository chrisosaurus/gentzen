module Gentzen.Check
(
    check,
)
where

import Gentzen.Data.WorkUnit
import qualified System.Data.System as System
import qualified System.Check as System
import qualified Proof.Data.Theorem as Proof
import qualified Proof.Check as Proof

check :: WorkUnit -> Either String ()
check (WorkUnit systems theorems) = do
    check_systems systems
    check_theorems theorems
    return ()

check_systems :: [System.System] -> Either String ()
check_systems [] = Right ()
check_systems (x:xs) = do
    check_system x
    check_systems xs
    return ()

check_system :: System.System -> Either String ()
check_system system = System.check system

check_theorems :: [Proof.Theorem] -> Either String ()
check_theorems [] = Right ()
check_theorems (x:xs) = do
    check_theorem x
    check_theorems xs
    return ()

check_theorem :: Proof.Theorem -> Either String ()
check_theorem theorem = Proof.check theorem

