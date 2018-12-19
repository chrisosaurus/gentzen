module Driver
(
    getFilename,
    readFileArgument,
    attemptLexing,
    attemptParsing,
    attemptCheck,
)
where

import System.Environment (getArgs)
import Lib

-- attempt to get a filename provided as an argument
getFilename :: IO String
getFilename = do
    args <- getArgs
    if length args == 0 || length args > 1
    then fail "Please provide a single argument which is the filename"
    else return (head args)

-- attempt to get filename provided as an argument
-- then attempt to read content of file
readFileArgument :: IO String
readFileArgument = do
    filename <- getFilename
    contents <- readFile filename
    return contents

attemptLexing :: String -> IO [Token]
attemptLexing contents = do
    case (lexer contents) of
        Left l -> fail $ "Lexing failed: " ++ l
        Right r -> return r

attemptParsing :: [Token] -> IO WorkUnit
attemptParsing tokens = do
    case (parse tokens) of
        Left l -> fail $ "Parsing failed: " ++ l
        Right r -> return r

attemptCheck :: WorkUnit -> IO ()
attemptCheck workunit = do
    case (check workunit) of
        Left l -> fail $ "Check failed: " ++ l
        Right () -> return ()

