{-# LANGUAGE NamedFieldPuns #-}

module Proof.Data.Proof
(
    ID,
    Step (..),
    Proof (..),
    start,
    finished,
    addSeq,
    addSeq',
    addSeqs,
    addSeqs',
    addStep,
    abortSeq,
    tip,
    tipN,
    get,
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
    deriving (Eq)

instance Show Proof where
    show Proof { sequents
               , steps
               , unproven
               , aborted
               , nextId
               } =   "\tunproven: "  ++ (show unproven)                           ++
                   "\n\taborted:  "  ++ (show (Set.toList aborted))               ++
                   "\n\tsequents:"   ++ (showSeqs  (Map.toList sequents))         ++
                   "\n\tsteps:"      ++ (showSteps (Map.toList steps))            ++
                   "\n"

showSeqs  :: [(ID, Sequent)] -> String
showSeqs [] = ""
showSeqs ((id, seq):xs) = this ++ rest
    where this = "\n\t\t" ++ (show id) ++ " : " ++ (show seq)
          rest = showSeqs xs

showSteps :: [(ID, Step)] -> String
showSteps [] = ""
showSteps ((id, step):xs) = this ++ rest
    where this = "\n\t\t" ++ (show id) ++ " : " ++ (show step)
          rest = showSteps xs

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

addSeq' :: Sequent -> Proof -> Proof
addSeq' seq proof = newProof
    where (ids, newProof) = addSeq seq proof

addSeqs :: [Sequent] -> Proof -> ([ID], Proof)
addSeqs [] proof = ([], proof)
addSeqs (seq:seqs) proof = (id:ids, finalProof)
    where (id, newProof)    = addSeq seq proof
          (ids, finalProof) = addSeqs seqs newProof

addSeqs' :: [Sequent] -> Proof -> Proof
addSeqs' seqs proof = newProof
    where (ids, newProof) = addSeqs seqs proof

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

tip :: Proof -> Either String ID
tip Proof { unproven = []    } =
    Left "Error: wanted '1' but only had '0' unproven sequents."
tip Proof { unproven = (x:_) } = Right x

-- I wish this could be Word -> Proof -> Either String [ID], but I don't want to pay
-- for conversion.
tipN :: Int -> Proof -> Either String [ID]
tipN n Proof { unproven } = if (length unproven) >= n
    then Right $ reverse $ take n unproven
    else Left  $ "Error: wanted '" ++ (show n) ++
                 "' but only had '" ++ (show (length unproven)) ++
                 "' unproven sequents."

get :: ID -> Proof -> Either String Sequent
get id Proof { sequents } = case Map.lookup id sequents of
    Nothing -> Left $ "Error: sequent with ID '" ++ (show id) ++ "' not found."
    Just s  -> Right s


