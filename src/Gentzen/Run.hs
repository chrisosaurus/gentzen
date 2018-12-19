module Gentzen.Run
(
    run,
)
where

import Gentzen.Data.WorkUnit
import qualified Proof.Run as Proof
import Proof.Data.SequentTree

run :: WorkUnit -> Either String ([String], SequentTree)
run (WorkUnit system theorem) = Proof.run system theorem

