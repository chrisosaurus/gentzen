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
check (WorkUnit system theorem) = do
    check_system system
    check_theorem theorem
    return ()

check_system :: System.System -> Either String ()
check_system system = System.check system

check_theorem :: Proof.Theorem -> Either String ()
check_theorem theorem = Proof.check theorem

