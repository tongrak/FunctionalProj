module AuxFunc (
    toLowerStr,
    strIsNum,
    semiColonD,
    stringD
) where

    import Data.Char (toLower, isDigit)
    import Tokenizer (Tokens, Token)

    toLowerStr:: String -> String
    toLowerStr = foldl (\acc x -> acc ++ [toLower x]) ""

    strIsNum:: String -> Bool
    strIsNum []  = False
    strIsNum tok = foldl aux True tok
        where aux acc x = if isDigit x then acc && True else False

    semiColonD:: Tokens -> Maybe (Tokens, Tokens)
    semiColonD ts = stringD ";" ts

    stringD:: String -> Tokens -> Maybe (Tokens, Tokens)
    stringD strD ts = aux [] ts
        where aux _ [] = Nothing
              aux acc (x:xs) = if x == strD then Just (acc++[x],xs) else aux (acc++[x]) xs