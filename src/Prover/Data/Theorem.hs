module Prover.Data.Theorem
(
    Stmt (..),
    Theorem (..),
)
where

import Sequent.Data.Sequent

data Stmt = Substeps [Stmt]
          -- apply a named rewrite rule with arguments
          | Apply String [Exp]
          -- Substeps are a bracketed subproof
          | Expect Sequent
          | Print
          | Abort
    deriving (Show, Eq)

-- theorem silly-axiom
-- system G3ip
-- sequent a |- a
-- proof
--   apply axiom [ a ]
-- qed

data Theorem = Theorem
    { name    :: String
    , system  :: String
    , sequent :: Sequent
    , steps   :: [Stmt]
    }
    deriving (Show, Eq)


