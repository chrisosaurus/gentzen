module Gentzen.Data.WorkUnit
(
    WorkUnit (..),
)
where

import qualified System.Data.System as System
import qualified Proof.Data.Theorem as Theorem

data WorkUnit = WorkUnit [System.System] [Theorem.Theorem]
    deriving (Show, Eq)

