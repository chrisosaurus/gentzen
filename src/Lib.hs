module Lib
(
    Token (..),
    lexer,
    parse,
    WorkUnit (..),
)
where

import Data.Token
import Lexer (lexer)
import Gentzen.Parser (parse)
import Gentzen.Data.WorkUnit

