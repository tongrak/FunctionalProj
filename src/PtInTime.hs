module PtInTime (
    dORrParse, dueDParse, reminDParse
) where

    import Tokenizer
    import TaskModel
    import AuxFunc

    dORrParse::Tokens -> Either String DnRobj
    dORrParse [] = Left "dNrParse: Empty Tokens"
    dORrParse (x:xs)
        | x == "BY" = ddParCon . dueDParse $ tokensToStr xs
        | x == "REMIND" = rmParCon . reminDParse $ tokensToStr xs
        | otherwise = Left "Unknown Point In Time"

    ddParCon::Either String DueDate -> Either String DnRobj
    ddParCon x = case x of
        Left ms  -> Left ms
        Right dd -> Right $ Left dd

    rmParCon::Either String Reminder -> Either String DnRobj
    rmParCon x = case x of
        Left ms  -> Left ms
        Right rm -> Right $ Right rm


    dueDParse::Token -> Either String DueDate
    dueDParse [] = Left "dueDParse: Empty Tokens"
    dueDParse ts = case splitTWith ':' ts of
        Nothing -> Left "dueDParse: DayOfWeek out of order"
        Just pa -> createDuedate pa

    -- dayOfWeekP:: String -> DueDate

    reminDParse::Token -> Either String Reminder
    reminDParse ts = case splitTWith 'h' ts of
        Nothing -> Left "rawReminDParse: Invalid reminder form"
        Just (fh, lh) -> 
            case dueDParse lh of
                Left ms -> Left ms
                Right dd ->createReminder fh dd 