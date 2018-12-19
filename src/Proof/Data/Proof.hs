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
    abortSeq,
)
where

import Sequent.Data.Sequent
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (delete)

type ID = Word

data Step  = Axiom    ID String [Exp]
           | Straight ID String [Exp] ID
           | Split    ID String [Exp] ID ID
           | Abort    ID
    deriving (Show, Eq)

data Proof = Proof { sequents :: Map.Map ID Sequent
                   , steps    :: Map.Map ID Step
                   -- unproven is a 'stack' of IDs, head is most recent.
                   , unproven :: [ID]
                   , aborted  :: Set.Set ID
                   , nextId   :: ID
                   }
    deriving (Show, Eq)

start :: Sequent -> Proof
start seq = Proof { sequents = Map.singleton goal_id seq
                  , steps    = Map.empty
                  , unproven = [goal_id]
                  , aborted  = Set.empty
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
                 , aborted
                 , nextId   = id
                 } = (id, newProof)
    where newProof = Proof { sequents = Map.insert id seq sequents
                           , steps
                           , unproven = id:unproven
                           , aborted
                           , nextId   = id+ 1
                           }


addStep :: Step -> Proof -> Proof
addStep step Proof { sequents
                   , steps
                   , unproven
                   , aborted
                   , nextId
                   } = newProof
    where newProof = Proof { sequents
                           , steps    = Map.insert fromId step steps
                           , unproven = delete fromId unproven
                           , aborted
                           , nextId
                           }
          fromId = case step of
                    Axiom    id _ _     -> id
                    Straight id _ _ _   -> id
                    Split    id _ _ _ _ -> id

abortSeq :: ID -> Proof -> Proof
abortSeq id Proof { sequents
                  , steps
                  , unproven
                  , aborted
                  , nextId
                  } = newProof
    where newProof = Proof { sequents
                           , steps
                           , unproven = delete id unproven
                           , aborted  = Set.insert id aborted
                           , nextId
                           }

