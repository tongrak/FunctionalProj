module CommPhaser (
    commPhase
) where

    import PtInTime
    import Tokenizer

    type Reminder = (PtOnClock, PtOnClen)
    type DueDate = PtOnClen
    type Tags = [[Char]]
    type Desc = String
    type Marked = Bool

    type Task = (Marked, Tags, Desc, (Maybe DueDate), (Maybe Reminder)) 
    type ParTask = ((Maybe Marked), Tags, (Maybe Desc), (Maybe DueDate), (Maybe Reminder))

    data Comm = Manipulate ManComm | Query QueComm| NotComm
        deriving (Show)
    data ManComm = CrComm Task | EdComm ParTask ParTask | DelComm ParTask
        deriving (Show)
    data QueComm = ShComm ParTask 
        deriving (Show)

    -- toLowerStr:: String -> String
    -- toLowerStr [] = []
    -- toLowerStr (x:xs) = toLower x:toLowerStr xs

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
        desc = tokeToStr tokens
        in (Manipulate(CrComm (False, tags, desc, maybDue, maybRemi)))
    
    tagFinder:: Tokens -> Tags
    tagFinder tokens = foldl tagFinderAux [] tokens

    tagFinderAux::Tags -> Token ->Tags
    tagFinderAux acc (x:xs) = if x == '#' then xs:acc else acc
    tagFinderAux acc [] = acc

    dueDateFinder x = Nothing

    reminFinder x = Nothing

    