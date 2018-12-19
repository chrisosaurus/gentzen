module System.Data.Rewrite
(
    Prop (..),
    Term (..),
    Expr (..),
    Sequ (..),
    Body (..),
    Rule (..),
)
where

import qualified Sequent.Data.Sequent as Sequent

-- a prosposition is a fact that can be checked
-- (a in L)
-- (_ in L)
data Prop = In Sequent.Exp String
    deriving (Show, Eq)

-- a term for rewriting one side of a sequent
-- each side can be any of
-- ()      |- ...
-- a       |- ...
-- (a^b)   |- ...
-- l-(a^v) |- ...
-- l-a+b   |- ...
data Term = Empty                          -- emptyset ()
          | SExp Sequent.Exp               -- naked expression (a^b) |- ...
          --     set-name    modifiers
          | Expr String      [Expr] -- l-a+b |- ...
    deriving (Show, Eq)

-- an expr is a single modification step applied to a Term
data Expr = Add    Sequent.Exp
          | Remove Sequent.Exp
    deriving (Show, Eq)

-- Terms for rewriting both side of a sequent
data Sequ = Sequ Term Term
    deriving (Show, Eq)

data Body = Unit
          | Single Sequ
          | Pair Sequ Sequ
    deriving (Show, Eq)

data Rule = Rule { rule_name  :: String
                 , args       :: [Sequent.Exp]
                 , left_name  :: String
                 , right_name :: String
                 , props      :: [Prop]
                 , body       :: Body
                 }
    deriving (Show, Eq)

