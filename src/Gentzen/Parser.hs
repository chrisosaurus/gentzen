module Gentzen.Parser
(
    parse,
)
where

import Data.Token
import ParserUtils
import qualified System.Parser as System
import qualified Proof.Parser as Proof
import Gentzen.Data.WorkUnit

parse :: [Token] -> Either String WorkUnit
parse tokens = do
    (tokens, system) <- System.parse_prefix tokens
    (tokens, proof) <- Proof.parse_prefix tokens
    [] <- expect_empty tokens
    return $ WorkUnit [system] [proof]

