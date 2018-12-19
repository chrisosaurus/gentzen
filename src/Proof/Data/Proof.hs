{-# LANGUAGE NamedFieldPuns #-}

module Proof.Data.Proof
(
    ID,
    Step (..),
    Proof (..),
    start,
    finished,
    addSeq,
    addStep,
)
where

import Sequent.Data.Sequent
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List

type ID = Word

data Step  = Axiom    ID String [Exp]
           | Straight ID String [Exp] ID
           | Split    ID String [Exp] ID ID
    deriving (Show, Eq)

data Proof = Proof { sequents :: Map.Map ID Sequent
                   , steps    :: Map.Map ID Step
                   , unproven :: Set.Set ID
                   , nextId   :: ID
                   }
    deriving (Show, Eq)

start :: Sequent -> Proof
start seq = Proof { sequents = Map.singleton goal_id seq
                  , steps    = Map.empty
                  , unproven = Set.singleton goal_id
                  , nextId   = 1
                  }
    where goal_id = 0

-- finished IFF unproven list is empty
finished :: Proof -> Bool
finished Proof {unproven=unproven} = null unproven

addSeq :: Sequent -> Proof -> (ID, Proof)
addSeq seq Proof { sequents
                 , steps
                 , unproven
                 , nextId   = id
                 } = (id, newProof)
    where newProof = Proof { sequents = Map.insert id seq sequents
                           , steps
                           , unproven = Set.insert id unproven
                           , nextId   = id+ 1
                           }


addStep :: Step -> Proof -> Proof
addStep step Proof { sequents
                   , steps
                   , unproven
                   , nextId
                   } = newProof
    where newProof = Proof { sequents
                           , steps    = Map.insert fromId step steps
                           , unproven = Set.delete fromId unproven
                           , nextId
                           }
          fromId = case step of
                    Axiom    id _ _     -> id
                    Straight id _ _ _   -> id
                    Split    id _ _ _ _ -> id

