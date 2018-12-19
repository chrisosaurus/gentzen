module System.Data.Env
(
    Env (..),
    exists,
    get,
    insert,
    remove,
    merge,
)
where

import qualified Sequent.Data.Sequent as Sequent

data Env = EnvEmpty
         | Env String [Sequent.Exp] Env
    deriving (Show, Eq)

exists :: Env -> String -> Bool
exists EnvEmpty str                  = False
exists (Env s _ rest) str | s == str = True
exists (Env _ _ rest) str            = exists rest str

get :: Env -> String -> Either String [Sequent.Exp]
get EnvEmpty str                  =
    Left $ "Env.get: environment lookup for '" ++ str ++ "' failed."
get (Env s e rest) str | s == str = Right e
get (Env _ _ rest) str            = get rest str

insert :: Env -> String -> [Sequent.Exp] -> Either String Env
insert env str _   | exists env str =
    Left $ "Env.insert: environment already contained '" ++ str ++ "'."
insert env str exp                  = Right $ Env str exp env

remove :: Env -> String -> Either String Env
remove (EnvEmpty) str =
    Left $ "Env.remove: environment did not contain '" ++ str ++ "'."
remove (Env s _ rest) str | s == str = Right rest
remove (Env s e rest) str             = do
    rest <- remove rest str
    return (Env s e rest)

merge :: Env -> Env -> Either String Env
merge (Env s e rest) right | exists right s = Left $ "Env.merge: duplicate key '" ++ s ++ "'."
merge (Env s e rest) right = do
    rest <- merge rest right
    return $ Env s e rest
merge EnvEmpty right = Right right

