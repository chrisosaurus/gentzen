module Main where

import Driver

main :: IO ()
main = do
    contents <- readFileArgument
    tokens <- attemptLexing contents

    putStrLn ""
    work <- attemptParsing tokens
    putStrLn "Parsing results:"
    putStrLn $ show work

    putStrLn ""
    attemptCheck work
    putStrLn "check successful"

    putStrLn ""
    attemptRun work
    putStrLn "run successful"

