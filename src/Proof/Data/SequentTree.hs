module Proof.Data.SequentTree
(
    SequentTree (..),
)
where

import Sequent.Data.Sequent
--                  Root is the first node in the tree
data SequentTree = Root      Sequent SequentTree
--                           rule   args    lout    lproof
                 | Step      String [Exp] [(Sequent, SequentTree)]
--                  Aborted is a proof that is purposefully unsatisfied
                 | Aborted
    deriving (Show, Eq)

