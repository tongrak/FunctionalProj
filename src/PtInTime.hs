module PtInTime (
    Min, Hou,
    Days, Date, Month, Year,
    PtOnClen, PtOnClock
) where

    import Tokenizer

    type Min = Int
    type Hou = Int

    -- daysStr = ["Monday", "Tueday", "Wednesday", ]

    data Days = Mon | Tue | Wed | Thu | Fri | Sat | Sun
        deriving (Show)
    type Date = Int
    data Month = Jan | Feb | Apl | May | Jun | Aug | Sep | Oct | Nov | Dec
        deriving (Show)
    type Year = Int

    type PtOnClock = (Hou, Min)
    type PtOnClen  = (Date, Month, Year)

    -- TokesToPtClk:: Tokens -> PtPtOnClock
    -- TokesToPtClk (x:xs)
    --         | 
