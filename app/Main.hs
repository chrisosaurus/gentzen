module Main where

import Lib
import Driver
import qualified Gentzen.Parser as Gentzen

main :: IO ()
main = do
    contents <- readFileArgument
    tokens <- attemptLexing contents
    work <- attemptParsing tokens
    putStrLn "Parsing results:"
    putStrLn $ show work
    attemptCheck work
    putStrLn "check successful"

