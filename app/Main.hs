module Main (main) where

-- import Lib
import Tokenizer
import CommParser (commParse, actOnComm)

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
                putStr "Error Dectected>"
                putStrLn m
                oneCommRunner
            Right c -> do
                putStr "Command recived>"
                print c
                do 
                    res <- actOnComm c
                    case res of
                        Left ms -> do
                            putStr "Error Dectected>"
                            putStrLn ms
                            return ()
                        Right () -> do
                            putStrLn "Command acted"
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