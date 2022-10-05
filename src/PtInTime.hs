module PtInTime (
    PtOnClen, PtOnClock
) where

    import Data.Time (TimeOfDay, DayOfWeek, toGregorian)
    -- import Data.Time.Calendar (MonthOfYear, Year)
    -- import Tokenizer

    type Min = Int
    type Hou = Int

    type PtOnClock = (Hou, Min)

    type Date = Int
    type MonthNum = Int
    type YearNum = Int

    type PtOnClen  = (Date, MonthNum, YearNum)

    -- TODO:
    -- clkPhase:: String -> Maybe PtOnClock 
        --  "10 am" -> (10,0)
        --  "1430" -> (14,30)

    -- clenPhase:: String -> Maybe PtOnClen
        --  "Mon" -> //Monday in current DayOfWeek
        --  "10 May" -> (10, 5, //Current Year)

    -- nextPtOnClen:: PtOnClen -> PtOnClen
        --  (24,9,2022) -> (31,10,2022)
        --  (28,10,2022) -> (4,11,2022)