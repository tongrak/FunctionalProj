module PtInTime (
    PtOnClen, PtOnClock, dNrParse
) where

    import Data.Time (TimeOfDay, DayOfWeek, toGregorian)
    -- import Data.Time.Calendar (MonthOfYear, Year)
    import Tokenizer
    import TaskModel

    type Min = Int
    type Hou = Int

    type PtOnClock = (Hou, Min)

    type Date = Int
    type MonthNum = Int
    type YearNum = Int

    type PtOnClen  = (Date, MonthNum, YearNum)

    dNrParse::Tokens -> DnRobj
    dNrParse _ = (0,0,0)

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