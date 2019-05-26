module Proof.Data.Theorem
(
    Stmt (..),
    Theorem (..),
)
where

import Sequent.Data.Sequent

          -- a Branch consists of two bracketed subproofs
data Stmt = Branch [Stmt] [Stmt]
          -- apply a named rewrite rule with arguments
          | Apply String [Exp]
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


