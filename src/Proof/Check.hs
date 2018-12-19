module Proof.Check
(
    check,
)
where

import Proof.Data.Theorem

-- We do not perform any checking of Theorems
-- if we knew the system and had it loaded, we could check argument arity and shape
-- but we will do that at runtime anyway.
check :: Theorem -> Either String ()
check _ = Right ()

