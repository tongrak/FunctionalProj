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
    tokenize str = filter (/= "") $ foldl tokenizeAux [] $ reverse str

    tokenizeAux:: [[Char]] -> Char -> [[Char]]
    tokenizeAux list inA 
        | isSpace inA   = []:list
        | inA == ';'    = []:";":list
        | otherwise     = addCLats list 
        where 
            addCLats (x:xs) = (inA:x):xs
            addCLats [] = [[inA]]

    tokensToStr:: Tokens -> String
    tokensToStr tokens = foldl1 aux tokens
        where aux acc x = acc ++ " " ++ x