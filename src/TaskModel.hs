module TaskModel(
    Task, ParTask, DnRobj, DueDate, Reminder, descToTask, taskfrom1Dnr, taskfrom2Dnr
) where

    type Date = String
    type Month = String
    type MiliTime = String

    type Reminder = (MiliTime, DueDate)
    type DueDate = (Date, Month)
    type Desc = String
    type Marked = Bool

    type DnRobj = Either DueDate Reminder

    data Task =  Task 
        {   mark::Bool,
            desc::String,
            fstDnR::Maybe DnRobj,
            sndDnR::Maybe DnRobj
        } deriving (Show)
    
    data ParTask = ParTaskCon 
        {   pMark::Maybe Bool,
            pDesc::Maybe String,
            pFstDnR::Maybe DnRobj,
            pSndDnR::Maybe DnRobj
        } deriving (Show)

    descToTask::String -> Task
    descToTask str = Task False str Nothing Nothing

    taskfrom1Dnr::String -> DnRobj -> Task
    taskfrom1Dnr str dnr = Task False str (Just dnr) Nothing

    taskfrom2Dnr::String -> DnRobj -> DnRobj -> Task
    taskfrom2Dnr str dnr1 dnr2 = Task False str (Just dnr1) (Just dnr2)