module Gentzen.Run
(
    run,
)
where

import Gentzen.Data.WorkUnit
import Proof.Data.Proof
import qualified Proof.Run2 as Proof

run :: WorkUnit -> Either String ([String], Proof)
run (WorkUnit system theorem) = Proof.run system theorem

