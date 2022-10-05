module Tokenizer (
    tokenize,
    tokensToStr,
    Token,
    Tokens
) where

    import Data.Char (isSpace)

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

    tokensToStr:: Tokens -> String
    tokensToStr tokens = foldl1 (++) tokens

