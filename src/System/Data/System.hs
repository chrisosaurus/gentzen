module System.Data.System
(
    System (..),
)
where

import Sequent.Data.Sequent
import System.Data.Rewrite

data System = System { system_name  :: String
                     , rules :: [Rule]
                     }
    deriving (Eq)

instance Show System where
    show (System name _) = "system '" ++ name ++ "'"

