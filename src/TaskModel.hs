module TaskModel(
    Task, ParTask, DueDate, Reminder, DnRobj, MarkStatue,
    isValidMili,
    createDuedate, createReminder,
    createMinTask, createDueTask, createReminTask, createDnRTask
) where

    -- text
    import Data.Text (Text, pack)
    import AuxFunc (strToNum)

    type Date = Int
    type Month = Int
    type MiliTime = String
    
    type DnRobj = Either DueDate Reminder

    data MarkStatue = Done | NotDone
        deriving (Show)
    data DueDate = Due (Date, Month) | NoDue
        deriving (Show)
    data Reminder = RemindOn (MiliTime, DueDate) | NoRemind
        deriving (Show)

    data Task =  Task 
        {   mark::MarkStatue,
            desc::Text,
            dueDate::DueDate,
            reminder::Reminder
        } deriving (Show)
    
    data ParTask = ParTask 
        {   pMark::MarkStatue, --Default as NotDone
            pDesc::Maybe Text,
            pFstDnR::Maybe DueDate,
            pSndDnR::Maybe Reminder
        } deriving (Show)

    isValidDate::Int -> Bool
    isValidDate x = x > 0 && x < 32

    isValidMonth::Int -> Bool
    isValidMonth x = x > 0 && x < 13

    isValidHour::Int -> Bool
    isValidHour x = x >= 0 && x < 25

    isValidMinu::Int -> Bool
    isValidMinu x = x >= 0 && x < 60

    createDuedate::(String, String) -> Either String DueDate
    createDuedate p = case  (\(a,b)->(strToNum a, strToNum b)) p of
        (Nothing, _)     -> Left "Invalid Date form"
        (_, Nothing)     -> Left "Invalid Month form"
        (Just d, Just m) -> aux d m
        where aux d m = if isValidDate d && isValidMonth m
                then Right $ Due (d, m)
                else Left "Invalid date or month"

    isValidMili::String -> Bool
    isValidMili str = if length str == 4
        then aux . (\(x,y)->(strToNum x, strToNum y)) . splitAt 2 $ str
        else False
        where aux p = case p of
                (Nothing, _) -> False
                (_, Nothing) -> False
                (Just a, Just b) -> isValidHour a && isValidMinu b

    createReminder::String -> DueDate -> Either String Reminder
    createReminder rm dd = if isValidMili rm
            then Right $ RemindOn (rm, dd)
            else Left "Invalid Militime form"

    createMinTask::String -> Task
    createMinTask str = createDnRTask str NoDue NoRemind

    createDueTask::String -> DueDate -> Task
    createDueTask str dd = createDnRTask str dd NoRemind

    createReminTask::String -> Reminder -> Task
    createReminTask str rm = createDnRTask str NoDue rm

    createDnRTask::String -> DueDate -> Reminder -> Task
    createDnRTask str dd rm = Task NotDone (pack str) dd rm