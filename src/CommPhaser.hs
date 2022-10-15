module CommPhaser (
    commParse, Comm, ManComm, QueComm
) where

    import Tokenizer as Tk
    import TaskModel as Tm
    import PtInTime

    data Comm = Manipulate ManComm | Query QueComm | NonComm
        deriving (Show)
    data ManComm = CrComm Task | EdComm ParTask ParTask | DelComm ParTask
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
        case (semiColonD ts) of
        Nothing -> Right ( Manipulate( CrComm (descToTask (tokensToStr ts))))
        Just (desTs, dNr) -> either 
            (\x -> Left x :: Either String Comm) 
            (\x -> Right (cCPAux (tokensToStr desTs) x)) 
            (dueNReParse dNr)
    
    cCPAux::String -> Either DnRobj (DnRobj,DnRobj) -> Comm
    cCPAux des bck = let
        task = case bck of
            Left o1 -> taskfrom1Dnr des o1
            Right (o1,o2) -> taskfrom2Dnr des o1 o2
        in Manipulate( CrComm (task))

    dueNReParse:: Tokens -> Either String (Either DnRobj (DnRobj,DnRobj))
    dueNReParse ts = let
        ei = case (semiColonD ts) of
                Nothing -> case dNrParse ts of
                    Left ms -> Left ms
                    Right dr -> Right (Left dr)
                Just (lf, rt) -> case biCheck of
                    Left ms -> Left ms
                    Right (lff, rtt) -> Right (Right (lff, rtt))
                    where biCheck = case (dNrParse lf, dNrParse rt) of
                        (Left mss, _) -> Left mss
                        (_, Left msx) -> Left msx
                        (Right a, Right b) -> Right(a, b)
                    
                    

    editCommParse:: Tokens -> Either String Comm
    editCommParse _ = Left "Temp"

    delCommParse:: Tokens -> Either String Comm
    delCommParse _ = Left "Temp"

    showCommParse:: Tokens -> Either String Comm
    showCommParse _ = Left "Temp"


    