module CommParser (
    actOnComm, commParse, Comm, AddComm, ManComm, QueComm
) where

    -- Base
    import Data.Text (pack)
    -- Local
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
      Manipulate mcc -> case mcc of
        EdComm _ _ -> return $ Left "Edit Command: Out of Order"
        DelComm dPT -> actOnDel dPT
      Query qcc -> case qcc of
        ShComm pT -> actOnShow pT
        ShowAll -> actOnShowAll
      NonComm -> return $ Left "can't act apon non-command"

    commParse:: Tokens -> Either String Comm
    commParse [] = Left "empty tokens"
    commParse (x:xs) 
            | x == "CREATE" = createCommParse xs
            | x == "EDIT" = editCommParse xs
            | x == "DELETE" = delCommParse xs
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
    delCommParse [] = Left "DelCommParse: No tasks detail detected"
    delCommParse ts = either (\x->Left x)
        (Right . Manipulate . DelComm)$ aParse ts

    showCommParse:: Tokens -> Either String Comm
    showCommParse [] = Right . Query $ ShowAll
    showCommParse ts = either (\x->Left x)
        (Right . Query . ShComm)$ aParse ts

    aParse:: Tokens -> Either String ParTask
    aParse ts = let emp = getEmPTask
        in loopA emp ts

    loopA::ParTask-> Tokens -> Either String ParTask
    loopA pt ts =  case splitTsSemi ts of
        Nothing-> bParse pt ts
        Just (curr, lef)->either (\x->Left x) (\x->loopA x lef)$ bParse pt curr

    bParse::ParTask-> Tokens-> Either String ParTask
    bParse _  [] = Left "bParse: Given tokens are empty"
    bParse pt (x:xs)
        | x == "MARK:Done" = Right $ changePTFrag (pt) (getPMark) (setPMark) (Just getDone)
        | x == "MARK:NotDone" = Right $ changePTFrag (pt) (getPMark) (setPMark) (Just getNotDone)
        | x == "NAMED" = Right . changePTFrag (pt) (getPDesc) (setPDesc) . Just . pack $ tokensToStr xs
        | x == "BY" = fmap (\a -> changePTFrag pt getPDD setPDD $ Just a) $ ddParse (x:xs)
        | x == "REMIND" = fmap (\a -> changePTFrag pt getPRe setPRe $ Just a) $ rmParse (x:xs)
        | otherwise = Left $ "Invalid fragment header :" ++ x

    changePTFrag::ParTask -> 
        (ParTask -> Maybe a) -> 
        (ParTask -> Maybe a -> ParTask) -> Maybe a -> ParTask
    changePTFrag pt getF setF toO = case getF pt of
        Nothing -> setF pt toO
        Just _ -> pt
    