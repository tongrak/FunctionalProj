module AuxFunc (
    toLowerStr,
    strIsNum
) where

    import Data.Char (toLower, isDigit)

    toLowerStr:: String -> String
    toLowerStr = foldl (\acc x -> acc ++ [toLower x]) ""

    strIsNum:: String -> Bool
    strIsNum []  = False
    strIsNum tok = foldl aux True tok
        where aux acc x = if isDigit x then acc && True else False