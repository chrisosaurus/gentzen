module Data.Sequent
(
    Exp (..),
    Sequent (..),
)
where

data Exp = And     Exp Exp
         | Or      Exp Exp
         | Implies Exp Exp
         | Bracked Exp

data Sequent = Sequent [Exp] [Exp]

