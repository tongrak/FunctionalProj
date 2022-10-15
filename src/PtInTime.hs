module PtInTime (
    PtOnClen, PtOnClock, dNrParse
) where

    import Data.Time (TimeOfDay, DayOfWeek, toGregorian)
    -- import Data.Time.Calendar (MonthOfYear, Year)
    import Tokenizer
    import TaskModel
    import AuxFunc

    type Min = Int
    type Hou = Int

    type PtOnClock = (Hou, Min)

    type Date = Int
    type MonthNum = Int
    type YearNum = Int

    type PtOnClen  = (Date, MonthNum, YearNum)

    dNrParse::Tokens -> Either String DnRobj
    dNrParse [] = Left "dNrParse: Empty Tokens"
    dNrParse (x:xs)
        | x == "BY" = Left $ dueDParse xs
        | x == "REMIND" = Right $ 
        | otherwise = Left "Unknown Point In Time"

    dueDParse::Tokens -> Either String PtOnClen
    dueDParse [] = Left "dueDParse: Empty Tokens"
    dueDParse ts = case stringD ":" ts of
        Nothing -> Right dayOfWeekP (tokensToStr ts)
        Just (fh, lh) -> Right (tkToNum fh, tkToNum lh, 2022)
        where tkToNum = strIsNum . tokensToStr

    dayOfWeekP:: String -> PtOnClen
    dayOfWeekP _ = (1,1,1)

    -- ptOnClenP::

    reminDParse::Tokens -> Either String (PtOnClock, PtOnClen)
    reminDParse ts = case stringD "h" ts of
        Nothing -> Left "reminDParse: Invalid form"
        Just (fh, lh) -> Right (ptOnClkP fh,dueDParse lh)

    ptOnClkP::String -> PtOnClock
    ptOnClkP _ = (1,1)

    -- TODO:
    -- clkPhase:: String -> Maybe PtOnClock 
        --  "10 am" -> (10,0)
        --  "1430" -> (14,30)

    -- clenPhase:: String -> Maybe PtOnClen
        --  "Mon" -> //Monday in current DayOfWeek
        --  "10 May" -> (10, 5, //Current Year)

    -- !! Will not work as intended.
    -- nextPtOnClen:: PtOnClen -> PtOnClen
        --  (24,9,2022) -> (31,9,2022)
        --  (28,10,2022) -> (4,11,2022)