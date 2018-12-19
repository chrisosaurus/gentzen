module Prover.Check
(
    check,
)
where

import Prover.Data.Theorem
import Prover.Data.SequentTree

check :: Theorem -> Either String String
check _ = Left "check: unimplemented"

