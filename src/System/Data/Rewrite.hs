module System.Data.Rewrite
(
    Prop (..),
    RewriteTerm (..),
    RewriteExpr (..),
    RewriteSequent (..),
    RewriteBody (..),
    RewriteRule (..),
)
where

import qualified Sequent.Data.Sequent as Sequent

-- (a in l)
-- TODO FIXME either have this as special syntax like this
--      [(a in l), (b in r)].()
--  or re-use the existing syntax, just have sequents which either fail or succeed
--      [l-a |- r-b].()
data Prop = In String String
    deriving (Show, Eq)

-- each side can be any of
-- ()      |- ...
-- a       |- ...
-- (a^b)   |- ...
-- l-(a^v) |- ...
-- l-a+b   |- ...
data RewriteTerm = Empty                          -- emptyset ()
                 | SExp Sequent.Exp               -- naked expression (a^b) |- ...
                 --     set-name    modifiers
                 | Expr String      [RewriteExpr] -- l-a+b |- ...
    deriving (Show, Eq)

data RewriteExpr = Add    Sequent.Exp
                 | Remove Sequent.Exp
    deriving (Show, Eq)

data RewriteSequent = RewriteSequent RewriteTerm RewriteTerm
    deriving (Show, Eq)

data RewriteBody = Unit
                 | Single RewriteSequent
                 | Pair RewriteSequent RewriteSequent
    deriving (Show, Eq)

data RewriteRule = RewriteRule { name       :: String
                               , args       :: [Sequent.Exp]
                               , left_name  :: String
                               , right_name :: String
                               , props      :: [Prop]
                               , body       :: RewriteBody
                               }
    deriving (Show, Eq)

