module Sequent.Data.Sequent
(
    Exp (..),
    Sequent (..),
)
where

import qualified Data.Token as Token

data Exp = And        Exp    Exp
         | Or         Exp    Exp
         | Implies    Exp    Exp
         | Symbol     String
         | Forall     String Exp
         | Exists     String Exp
         | Subst      Exp    Exp String
         | Bottom
    deriving (Eq)

data Sequent = Sequent [Exp] [Exp]
    deriving (Eq)

instance Show Exp where
    show (And l r)             = (show l) ++ (show Token.And)  ++ (show r)
    show (Or l r)              = (show l) ++ (show Token.Or)  ++ (show r)
    show (Implies l r)         = (show l) ++ (show Token.Implies) ++ (show r)
    show (Symbol s)            = s
    show (Forall x e)          = (show Token.Forall) ++ (show x) ++ " " ++ (show e)
    show (Exists x e)          = (show Token.Exists) ++ (show x) ++ " " ++ (show e)
    show (Subst e1 e2 v)  = (show e1) ++ (show Token.LessThan) ++ (show e2) ++ (show Token.ForwardSlash) ++ v ++ (show Token.GreaterThan)
    show (Bottom)              = show Token.Bottom

instance Show Sequent where
    show (Sequent lhs rhs) = (show lhs) ++ " |- " ++ (show rhs)
