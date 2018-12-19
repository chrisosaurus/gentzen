module Prover.Data.Theorem
(
    Stmt (..),
    Theorem (..),
)
where

import Sequent.Data.Sequent

data Stmt = Axiom String
          -- Substeps are a bracketed subproof
          | Substeps [Stmt]
          | Expect Sequent
          | Print
          | Abort
    deriving (Show, Eq)

-- theorem silly-axiom
-- system G3ip
-- sequent a |- a
-- proof
--   axiom a
-- qed

data Theorem = Theorem
    { name    :: String
    , system  :: String
    , sequent :: Sequent
    , steps   :: [Stmt]
    }
    deriving (Show, Eq)


