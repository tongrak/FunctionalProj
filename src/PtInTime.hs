module PtInTime (
    dORrParse, ddParse, rmParse
) where

    import Tokenizer
    import TaskModel
    import AuxFunc

    dORrParse::Tokens -> Either String DnRobj
    dORrParse [] = Left "dORrParse: Empty Tokens"
    dORrParse (x:xs)
        | x == "BY" = ddParCon . dueDParse $ tokensToStr xs
        | x == "REMIND" = rmParCon . reminDParse $ tokensToStr xs
        | otherwise = Left $ "dORrParse: Invalid header" ++ x

    ddParse::Tokens -> Either String DueDate
    ddParse [] = Left "DdParse: Empty Tokens"
    ddParse (x:xs)
        | x == "BY" = dueDParse . tokensToStr $ xs
        | otherwise = Left $ "ddParse: Invalid header" ++ x

    rmParse::Tokens -> Either String Reminder
    rmParse [] = Left "rmParse: Empty Tokens"
    rmParse (x:xs)
        | x == "REMIND" = reminDParse . tokensToStr $ xs
        | otherwise = Left $ "rmParse: Invalid header" ++ x


    ddParCon::Either String DueDate -> Either String DnRobj
    ddParCon x = x >>= (Right . Left)

    rmParCon::Either String Reminder -> Either String DnRobj
    rmParCon x = x >>= (Right . Right)

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