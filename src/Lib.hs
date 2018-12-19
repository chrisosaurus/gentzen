module Lib
(
    lexer,
    Token (..),
    parse,
    WorkUnit (..),
    check,
)
where

import Lexer (lexer)
import Data.Token (Token)
import Gentzen.Parser (parse)
import Gentzen.Data.WorkUnit (WorkUnit)
import Gentzen.Check (check)


