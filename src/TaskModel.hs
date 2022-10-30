{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module TaskModel(
    Task, ParTask, DueDate, Reminder, DnRobj, MarkStatue,
    isValidMili, getDone, getNotDone,
    createDuedate, createReminder,
    createMinTask, createDueTask, createReminTask, createDnRTask,
    taskHeader, 
    getMark, getDesc, getDueDate, getReminder,
    getPMark, getPDesc, getPDD, getPRe,
    setPMark, setPDesc, setPDD, setPRe,
    getEmPTask
) where

    -- text
    import Data.Text (Text, pack)
    -- cassava
    import Data.Csv
        ( DefaultOrdered(headerOrder)
        , FromField(parseField)
        , FromNamedRecord(parseNamedRecord)
        , Header
        , ToField(toField)
        , ToNamedRecord(toNamedRecord)
        , (.:)
        , (.=)
        )
    import qualified Data.Csv as Cassava
    -- bytestring
    import Data.ByteString.Lazy (ByteString)
    import Data.ByteString.Internal (packChars, unpackChars)
    import qualified Data.ByteString.Lazy as ByteString
    -- Vector
    import Data.Vector (Vector)
    import qualified Data.Vector as Vector
    -- Local
    import AuxFunc (strToNum, splitTWith)

    taskHeader:: Header
    taskHeader = Vector.fromList ["Marked", "Description", "DueDate", "Reminder"]

    instance DefaultOrdered Task where
        headerOrder _ = Cassava.header ["Marked", "Description", "DueDate", "Reminder"]

    type Date = Int
    type Month = Int
    type MiliTime = String
    
    type DnRobj = Either DueDate Reminder

    data MarkStatue = Done | NotDone
        deriving (Eq, Show)
    data DueDate = Due (Date, Month) | NoDue
        deriving (Eq)
    data Reminder = RemindOn (MiliTime, DueDate) | NoRemind
        deriving (Eq)

    getDone:: MarkStatue
    getDone = Done
    getNotDone:: MarkStatue
    getNotDone = NotDone

    instance Show DueDate where
        show (Due dd) = (show.fst$dd) ++ ":" ++ (show.snd$dd)
        show NoDue = "NoDue"
    
    instance Show Reminder where
        show (RemindOn rm) = (fst rm) ++ "h" ++ (show.snd$rm)
        show NoRemind = "NoRemind"

    data Task =  Task 
        {   mark::MarkStatue,
            desc::Text,
            dueDate::DueDate,
            reminder::Reminder
        } deriving (Eq)
    
    getMark::Task -> MarkStatue
    getMark = mark
    getDesc::Task -> Text
    getDesc = desc
    getDueDate::Task -> DueDate
    getDueDate = dueDate
    getReminder::Task -> Reminder
    getReminder = reminder

    instance Show Task where
        show task = "Task: Mark:" ++ (show .mark$ task) 
            ++ ", Des:" ++ (show .desc$task) ++ ", DueDate:" 
            ++ (show .dueDate$task) ++ ", Remind:" ++ (show .reminder$task)
    
    data ParTask = ParTask 
        {   pMark::Maybe MarkStatue, --Default as NotDone
            pDesc::Maybe Text,
            pDueDate::Maybe DueDate,
            pRemind::Maybe Reminder
        } deriving (Eq, Show)

    getEmPTask::ParTask
    getEmPTask = ParTask Nothing Nothing Nothing Nothing

    getPMark::ParTask->Maybe MarkStatue
    getPMark = pMark
    setPMark::ParTask->Maybe MarkStatue->ParTask
    setPMark pt mar = pt {pMark = mar}
    getPDesc::ParTask->Maybe Text
    getPDesc = pDesc
    setPDesc::ParTask->Maybe Text->ParTask
    setPDesc pt t = pt {pDesc = t}
    getPDD::ParTask->Maybe DueDate
    getPDD = pDueDate
    setPDD::ParTask->Maybe DueDate->ParTask
    setPDD pt dd = pt {pDueDate = dd}
    getPRe::ParTask->Maybe Reminder
    getPRe = pRemind
    setPRe::ParTask->Maybe Reminder->ParTask
    setPRe pt rm = pt {pRemind = rm}

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

    instance FromNamedRecord Task where
        parseNamedRecord m = Task 
            <$> m .: "Marked" 
            <*> m .: "Description" 
            <*> m .: "DueDate"
            <*> m .: "Reminder"

    instance FromField MarkStatue where
        parseField "Done" = return Done
        parseField _ = return NotDone
    
    unpackDDstr::String ->DueDate
    unpackDDstr str = case splitTWith ':' str of
        Nothing -> NoDue
        Just (ho, mi) -> case (strToNum ho, strToNum mi) of
            (Nothing, _) -> NoDue
            (_, Nothing) -> NoDue
            (Just h, Just m) -> Due (h,m)

    unpackRMstr::String ->Reminder
    unpackRMstr str = case splitTWith 'h' str of
        Nothing -> NoRemind
        Just (rm, ndd) -> RemindOn (rm, unpackDDstr ndd)

    instance FromField DueDate where
        parseField "NoDue" = return NoDue
        parseField dd = parseField  dd >>= return . unpackDDstr . (filter (/= '"'))
    
    instance FromField Reminder where
        parseField "NoRemind" = return NoRemind
        parseField rm = parseField  rm >>= return . unpackRMstr . (filter (/= '"'))

    instance ToNamedRecord Task where
        toNamedRecord Task{..} = Cassava.namedRecord
            [   "Marked"        .= mark,
                "Description"   .= desc,
                "DueDate"       .= dueDate,
                "Reminder"      .= reminder
            ]

    instance ToField MarkStatue where
        toField Done = "Done"
        toField NotDone = "NotDone"

    instance ToField DueDate where
        toField dd@(Due _) = packChars.show $ dd
        toField NoDue = "NoDue"

    instance ToField Reminder where
        toField rm@(RemindOn _) = packChars.show $ rm
        toField NoRemind = "NoRemind"