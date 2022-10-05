module CommPhaser (
    commPhase, Comm, ManComm, QueComm
) where

    import Tokenizer as Tk
    import TaskModel

    data Comm = Manipulate ManComm | Query QueComm| NotComm
        deriving (Show)
    data ManComm = CrComm Task | EdComm ParTask ParTask | DelComm ParTask
        deriving (Show)
    data QueComm = ShComm ParTask 
        deriving (Show)

    commPhase:: Tokens -> Comm
    commPhase [] = NotComm
    commPhase (x:xs) 
            | x == "CREATE" = cComPhaser xs
            | x == "EDIT" = NotComm
            | x == "DELECTE" = NotComm
            | x == "SHOW" = NotComm
            | otherwise = NotComm

    cComPhaser:: Tokens -> Comm
    cComPhaser tokens = let 
        tags = tagFinder tokens
        maybDue = dueDateFinder tokens
        maybRemi = reminFinder tokens
        desc = Tk.tokensToStr tokens
        in (Manipulate(CrComm (False, tags, desc, maybDue, maybRemi)))
    
    tagFinder:: Tokens -> [String]
    tagFinder tokens = foldl tagFinderAux [] tokens

    tagFinderAux::[String] -> Token ->[String]
    tagFinderAux acc (x:xs) = if x == '#' then xs:acc else acc
    tagFinderAux acc [] = acc

    dueDateFinder x = Nothing

    reminFinder x = Nothing

    