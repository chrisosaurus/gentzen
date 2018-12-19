module Proof.Check
(
    check,
)
where

import Proof.Data.Theorem
import Proof.Data.SequentTree

check :: Theorem -> Either String String
check _ = Left "check: unimplemented"

