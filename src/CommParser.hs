module CommParser (
    commParse, Comm, AddComm, ManComm, QueComm
) where

    import Tokenizer
    import TaskModel
    import AuxFunc
    import PtInTime

    data Comm = Add AddComm | Manipulate ManComm | Query QueComm | NonComm
        deriving (Show)
    data AddComm =  CrComm Task
        deriving (Show)
    data ManComm =  EdComm ParTask ParTask | DelComm ParTask
        deriving (Show)
    data QueComm = ShComm ParTask 
        deriving (Show)

    commParse:: Tokens -> Either String Comm
    commParse [] = Left "empty tokens"
    commParse (x:xs) 
            | x == "CREATE" = createCommParse xs
            | x == "EDIT" = editCommParse xs
            | x == "DELECTE" = delCommParse xs
            | x == "SHOW" = showCommParse xs
            | otherwise = Left "unknown command"

    createCommParse:: Tokens -> Either String Comm
    createCommParse ts = 
        case (splitTsSemi ts) of
        Nothing -> Right ( Add( CrComm (descToTask (tokensToStr ts))))
        Just (desTs, dNr) -> either 
            (\x -> Left x :: Either String Comm) 
            (\x -> Right (cCPAux (tokensToStr desTs) x)) 
            (dueNReParse dNr)
    
    cCPAux::String -> Either DnRobj (DnRobj,DnRobj) -> Comm
    cCPAux des bck = let
        task = case bck of
            Left o1 -> taskfrom1Dnr des o1
            Right (o1,o2) -> taskfrom2Dnr des o1 o2
        in Add ( CrComm (task))

    dueNReParse:: Tokens -> Either String (Either DnRobj (DnRobj,DnRobj))
    dueNReParse ts = case (splitTsSemi ts) of
        Nothing -> dNrPCase ts
        Just p -> case biDnRCase p of
            Left ms -> Left ms
            Right pairDnR -> Right (Right pairDnR)
                    
    dNrPCase::Tokens -> Either String (Either DnRobj (DnRobj,DnRobj))
    dNrPCase ts = case dNrParse ts of
                    Left ms -> Left ms
                    Right dr -> Right (Left dr)

    biDnRCase::(Tokens, Tokens) -> Either String (DnRobj,DnRobj)
    biDnRCase (lTs, rTs) = let
        lDnR = dNrParse lTs
        rDnR = dNrParse rTs
        in case (lDnR, rDnR) of
                    (Left mss, _) -> Left mss
                    (_, Left msx) -> Left msx
                    (Right a, Right b) -> Right(a, b)

    editCommParse:: Tokens -> Either String Comm
    editCommParse _ = Left "Out of order"

    delCommParse:: Tokens -> Either String Comm
    delCommParse _ = Left "Out of order"

    showCommParse:: Tokens -> Either String Comm
    showCommParse _ = Left "Out of order"