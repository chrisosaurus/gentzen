module Gentzen.Run
(
    run,
)
where

import Gentzen.Data.WorkUnit

-- TODO FIXME this interface is likely wrong, SequentTree?
run :: WorkUnit -> Either String ()
run _ = Left "run unimplemented"
