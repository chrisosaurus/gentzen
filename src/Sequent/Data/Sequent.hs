module Sequent.Data.Sequent
(
    Exp (..),
    Sequent (..),
)
where

data Exp = And       Exp Exp
         | Or        Exp Exp
         | Implies   Exp Exp
         | Bracketed Exp
         | Symbol    String
    deriving (Show, Eq)

data Sequent = Sequent [Exp] [Exp]
    deriving (Show, Eq)
