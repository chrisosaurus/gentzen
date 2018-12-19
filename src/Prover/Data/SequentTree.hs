module Prover.Data.SequentTree
(
    SequentTree (..),
)
where

import Sequent.Data.Sequent

data SequentTree = Unary  Sequent String SequentTree
                 | Binary Sequent String SequentTree SequentTree
                 | Axiom  String
    deriving (Show, Eq)

