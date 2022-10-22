module AuxFunc (
    toLowerStr,
    strIsNum,
    splitTsSemi,
    splitTWith
) where

    import Data.Char (toLower, isDigit)
    import Tokenizer (Tokens, Token)

    toLowerStr:: String -> String
    toLowerStr = foldl (\acc x -> acc ++ [toLower x]) ""

    strIsNum:: String -> Bool
    strIsNum []  = False
    strIsNum tok = foldl aux True tok
        where aux acc x = if isDigit x then acc && True else False

    -- strToNum:: String -> Maybe Int
    -- strToNum [] = Nothing
    -- strToNum x = 

    splitTsSemi:: Tokens -> Maybe (Tokens, Tokens)
    splitTsSemi ts = splitTsWith ";" ts

    splitTsWith:: String -> Tokens -> Maybe (Tokens, Tokens)
    splitTsWith strD ts = aux [] ts
        where aux _ [] = Nothing
              aux acc (x:xs) = if x == strD then Just (acc,xs) else aux (acc++[x]) xs

    splitTWith:: Char -> Token -> Maybe (Token, Token)
    splitTWith key t = aux [] t
        where aux _ [] = Nothing
              aux acc (x:xs) = if x == key then Just (acc,xs) else aux (acc++[x]) xs