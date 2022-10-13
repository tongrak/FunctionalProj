module Main (main) where

-- import Lib
import Tokenizer
import CommPhaser

main :: IO ()
main = do oneCommRunner

oneCommRunner:: IO ()
oneCommRunner = do
    line <- printNGet "insert command >"
    let tokens@(x:_) = tokenize line
    if x == "exit"
    then return ()
    else do -- run 
        putStr "Tokens>"
        print tokens
        let pcomm = commParse tokens
        case pcomm of
            Left m -> do
                putStrLn m
                oneCommRunner
            Right _ -> do
                putStrLn "Command recived"
                return ()

printNGet::String -> IO String
printNGet msg = do
    putStr msg
    line <- getLine
    if line == []
    then do
        putStr "no input detected"
        printNGet msg
    else return line