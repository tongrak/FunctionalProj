module TaskModel(
    Task, ParTask
) where

    import PtInTime

    type Reminder = (PtOnClock, PtOnClen)
    type DueDate = PtOnClen
    type Tags = [[Char]]
    type Desc = String
    type Marked = Bool

    type Task = (Marked, Tags, Desc, (Maybe DueDate), (Maybe Reminder)) 
    type ParTask = ((Maybe Marked), Tags, (Maybe Desc), (Maybe DueDate), (Maybe Reminder))