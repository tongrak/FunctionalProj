module TaskModel(
    Task, ParTask, DnRobj, descToTask, taskfrom1Dnr, taskfrom2Dnr
) where

    import PtInTime

    type Reminder = (PtOnClock, PtOnClen)
    type DueDate = PtOnClen
    -- type Tags = [[Char]]
    type Desc = String
    type Marked = Bool

    type DnRobj = Either DueDate Reminder

    data Task =  TaskCon 
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
    descToTask str = TaskCon False str Nothing Nothing

    taskfrom1Dnr::String -> DnRobj -> Task
    taskfrom1Dnr str dnr = TaskCon False str (Just dnr) Nothing

    taskfrom2Dnr::String -> DnRobj -> DnRobj -> Task
    taskfrom2Dnr str dnr1 dnr2 = TaskCon False str (Just dnr1) (Just dnr2)


    -- type Task = (Marked, Desc, (Maybe DueDate), (Maybe Reminder)) 
    -- type ParTask = ((Maybe Marked), (Maybe Desc), (Maybe DueDate), (Maybe Reminder))