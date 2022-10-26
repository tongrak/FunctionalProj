module TaskModel(
    Task, ParTask, DueDate, Reminder, MarkStatue,
    createDuedate, createReminder,
    createMinTask, createDueTask, createReminTask, createDnRTask
) where

    -- text
    import Data.Text (Text, pack)
    import AuxFunc (strToNum)

    type Date = Int
    type Month = Int
    type MiliTime = String

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
    isValidHour x = x > 0 && x < 25

    isValidMinu::Int -> Bool
    isValidMinu x = x > 0 && x < 60

    createDuedate::Int -> Int -> Maybe DueDate
    createDuedate d m = if isValidDate d && isValidMonth m
        then Just $ Due (d, m)
        else Nothing

    isValidMili::String -> Bool
    isValidMili str = if length str == 4
        then aux . (\(x,y)->(strToNum x, strToNum y)) . splitAt 2 $ str
        else False
        where aux p = case p of
                (Nothing, _) -> False
                (_, Nothing) -> False
                (Just a, Just b) -> isValidHour a && isValidMinu b

    createReminder::String -> DueDate -> Maybe Reminder
    createReminder rm dd = if isValidMili rm
            then Just $ RemindOn (rm, dd)
            else Nothing

    createMinTask::String -> Task
    createMinTask str = createDnRTask str NoDue NoRemind

    createDueTask::String -> DueDate -> Task
    createDueTask str dd = createDnRTask str dd NoRemind

    createReminTask::String -> Reminder -> Task
    createReminTask str rm = createDnRTask str NoDue rm

    createDnRTask::String -> DueDate -> Reminder -> Task
    createDnRTask str dd rm = Task NotDone (pack str) dd rm