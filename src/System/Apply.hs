module System.Apply
(
    apply,
)
where

import Sequent.Data.Sequent
import System.Data.Rewrite
import System.Data.Rule

apply :: Sequent -> Rule -> Either String Sequent
apply _ _ = Left "System.Apply.apply unimplemented"
