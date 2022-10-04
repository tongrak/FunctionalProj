module Tokenizer (
    tokenize,
    tokeToStr,
    tokeIsNum,
    Token,
    Tokens
) where

    import Data.Char

    type Token = [Char]
    type Tokens = [Token]

    tokenize::String -> Tokens
    tokenize [] = []
    tokenize list = foldl tokenizeAux [] $ reverse list

    tokenizeAux:: [[Char]] -> Char -> [[Char]]
    tokenizeAux list inA 
        | isSpace  inA  = []:list
        | otherwise     = addCLats list 
        where 
            addCLats (x:xs) = (inA:x):xs
            addCLats [] = [[inA]]

    tokeToStr:: Tokens -> String
    tokeToStr tokens = foldl1 (++) tokens

    tokeIsNum:: Token -> Bool
    tokeIsNum []  = False
    tokeIsNum tok = foldl aux True tok
        where aux acc x = if isDigit x then acc && True else False
