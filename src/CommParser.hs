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
    data QueComm = ShComm ParTask | ShowAll
        deriving (Show)

    actOnComm::Comm-> IO (Either String ())
    actOnComm comm = case comm of
      Add (CrComm task) -> actOnCrComm task
      Manipulate _ -> return $ Left "Manipulate Comm: Out of order"
      Query qcc -> case qcc of
        ShComm _ -> return $ Left "Out of order"
        ShowAll -> actOnShowAll
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
        Nothing -> Right . Add . CrComm . createMinTask . tokensToStr $ ts
        Just (desTs, dNr) -> either 
            (\x -> Left x :: Either String Comm) 
            (\x -> Right (cCPAux (tokensToStr desTs) x)) 
            (dueNReParse dNr)
    
    cCPAux::String -> Either DnRobj (DueDate,Reminder) -> Comm
    cCPAux des bck = let
        task = case bck of
            Left o1 -> either (createDueTask des) (createReminTask des) o1
            Right (dd,rm) -> createDnRTask des dd rm
        in Add ( CrComm (task))

    dueNReParse:: Tokens -> Either String (Either DnRobj (DueDate,Reminder))
    dueNReParse ts = case (splitTsSemi ts) of
        Nothing -> either (\x->Left x) (\x-> Right .Left$ x) (dORrParse ts)
        Just (d, r) -> case (ddParse d, rmParse r) of
            (Left ms, _) -> Left ms
            (_, Left ms) -> Left ms
            (Right dd, Right rm) -> Right $ Right (dd,rm)

    editCommParse:: Tokens -> Either String Comm
    editCommParse _ = Left "Out of order"

    delCommParse:: Tokens -> Either String Comm
    delCommParse _ = Left "Out of order"

    showCommParse:: Tokens -> Either String Comm
    showCommParse [] = Right . Query $ ShowAll
    showCommParse ts = either (\x->Left x)
        (Right . Query . ShComm)$ aParse ts

    aParse:: Tokens -> Either String ParTask
    aParse ts = let emp = getEmPTask
        in loopA emp ts

    loopA::ParTask-> Tokens -> Either String ParTask
    loopA pt ts =  case splitTsSemi ts of
        Nothing-> passToB
        Just (curr, lef)->either (\x->Left x) (\x->loopA x lef)$ passToB
        where passToB = bParse pt ts

    bParse::ParTask-> Tokens-> Either String ParTask
    bParse _  [] = Left "bParse: Tokens are empty"
    bParse pt (x:xs)
        | x == "MARK:Done" = Right $ changePTFrag (pt) (getPMark) (setPMark) (Just getDone)
        | x == "MARK:NotDone" = Right $ changePTFrag (pt) (getPMark) (setPMark) (Just getNotDone)
        | otherwise = Left $ "Invalid partial task header" ++ x

    isNothing:: Maybe a -> Bool
    isNothing x = case x of
        Nothing -> True
        Just _  -> False

    changePTFrag::ParTask -> 
        (ParTask -> Maybe a) -> 
        (ParTask -> Maybe a -> ParTask) -> Maybe a -> ParTask
    changePTFrag pt getF setF toO = case getF pt of
        Nothing -> setF pt toO
        Just _ -> pt
    