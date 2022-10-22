module PtInTime (
    dNrParse
) where

    import Tokenizer
    import TaskModel
    import AuxFunc

    dNrParse::Tokens -> Either String DnRobj
    dNrParse [] = Left "dNrParse: Empty Tokens"
    dNrParse (x:xs)
        | x == "BY" = dueDParse $ tokensToStr xs
        | x == "REMIND" = reminDParse $ tokensToStr xs
        | otherwise = Left "Unknown Point In Time"

    dueDParse::Token -> Either String DnRobj
    dueDParse ts = case rawDueDParse ts of
        Left ms -> Left ms
        Right dd -> Right $ Left dd

    rawDueDParse::Token -> Either String DueDate
    rawDueDParse [] = Left "dueDParse: Empty Tokens"
    rawDueDParse ts = case splitTWith ':' ts of
        Nothing -> Left "dueDParse: DayOfWeek out of order"
        Just (f,t) -> Right (f,filter aux  t)
        where aux c = c /= ';' && c /= ' '

    -- dayOfWeekP:: String -> PtOnClen

    reminDParse::Token -> Either String DnRobj
    reminDParse ts = case splitTWith 'h' ts of
        Nothing -> Left "reminDParse: Invalid reminder form"
        Just (mTime, lh) -> let
            lhResult = rawDueDParse lh
            in case lhResult of
                Left ms -> Left ms
                Right dd -> if length mTime == 4 
                    then Right $ Right (mTime, dd)
                    else Left $ "reminDParse: input invalid militime form: " ++ mTime
