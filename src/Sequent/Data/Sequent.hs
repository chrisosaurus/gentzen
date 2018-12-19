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
    deriving (Eq)

data Sequent = Sequent [Exp] [Exp]
    deriving (Eq)

instance Show Exp where
    show (And l r)     = (show l) ++ "^"  ++ (show r)
    show (Or l r)      = (show l) ++ "v"  ++ (show r)
    show (Implies l r) = (show l) ++ "->" ++ (show r)
    show (Bracketed e) = "(" ++ (show e) ++ ")"
    show (Symbol s)    = s

instance Show Sequent where
    show (Sequent lhs rhs) = (show lhs) ++ " |- " ++ (show rhs)
