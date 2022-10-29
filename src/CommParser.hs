module CommParser (
    actOnComm, commParse, Comm, AddComm, ManComm, QueComm
) where

    import Tokenizer
    import TaskModel
    import AuxFunc
    import PtInTime
    import CommEnforcer

    data Comm = Add AddComm | Manipulate ManComm | Query QueComm | NonComm
        deriving (Show)
    data AddComm =  CrComm Task
        deriving (Show)
    data ManComm =  EdComm ParTask ParTask | DelComm ParTask
        deriving (Show)
    data QueComm = ShComm ParTask 
        deriving (Show)

    actOnComm::Comm-> IO (Either String ())
    actOnComm comm = case comm of
      Add (CrComm task) -> actOnCrComm task
      Manipulate _ -> return $ Left "Manipulate Comm: Out of order"
      Query _ -> return $ Left "Query Comm: Out of order"
      NonComm -> return $ Left "can't act apon non-command"

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
        Nothing -> Right ( Add( CrComm (createMinTask (tokensToStr ts))))
        Just (desTs, dNr) -> either 
            (\x -> Left x :: Either String Comm) 
            (\x -> Right (cCPAux (tokensToStr desTs) x)) 
            (dueNReParse dNr)
    
    cCPAux::String -> Either DnRobj (DueDate,Reminder) -> Comm
    cCPAux des bck = let
        task = case bck of
            Left o1 -> aux o1 
            Right (dd,rm) -> createDnRTask des dd rm
        in Add ( CrComm (task))
        where aux dNr = case dNr of
                Left dd  -> createDueTask des dd
                Right rm -> createReminTask des rm

    dueNReParse:: Tokens -> Either String (Either DnRobj (DueDate,Reminder))
    dueNReParse ts = case (splitTsSemi ts) of
        Nothing -> case dORrParse ts of
            Left ms -> Left ms
            Right dNr -> Right $ Left $ dNr
        Just (d, r) -> case (ddParse d, rmParse r) of
            (Left ms, _) -> Left ms
            (_, Left ms) -> Left ms
            (Right dd, Right rm) -> Right $ Right (dd,rm)

    editCommParse:: Tokens -> Either String Comm
    editCommParse _ = Left "Out of order"

    delCommParse:: Tokens -> Either String Comm
    delCommParse _ = Left "Out of order"

    showCommParse:: Tokens -> Either String Comm
    showCommParse _ = Left "Out of order"