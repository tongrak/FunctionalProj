module Main (main) where

import Lib
import Tokenizer
import CommPhaser

main :: IO ()
main = someFunc

oneCommRunner:: IO ()
oneCommRunner = do
    line <- printNGet "insert command >"
    let (x:xs) = tokenize line
    if x == "exit"
        then return ()
        else do -- run 
            putStr "Tokens>"
            print (x:xs)
            let comm = commPhase (x:xs)
            print comm
            -- actOnComm comm
            return ()

printNGet::String -> IO String
printNGet msg = do
    putStr msg
    line <- getLine
    return line