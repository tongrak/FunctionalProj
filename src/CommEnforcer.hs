-- extensions
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CommEnforcer (
  actOnCrComm, actOnShowAll, actOnShow, actOnDel
) where
  -- Vector
  import Data.Vector (Vector, fromList, filter)
  import qualified Data.Vector as Vector
  -- ByteString
  import Data.ByteString.Lazy (ByteString)
  import Data.ByteString.Internal (packChars, unpackChars)
  import qualified Data.ByteString.Lazy as ByteString
  -- Cassava
  import qualified Data.Csv as Cassava
  -- Base
  import Data.Foldable (toList)
  import Control.Exception (IOException)
  import qualified Control.Exception as Exception
  import qualified Data.Foldable as Foldable
  import Data.Text (Text, pack, unpack, isInfixOf)
  import Data.String
  -- local
  import TaskModel

  filePath = "csv/tasks.csv"

  catchShowIO:: IO a-> IO (Either String a)
  catchShowIO action =fmap Right action `Exception.catch` handleIOException
    where
      handleIOException:: IOException-> IO (Either String a)
      handleIOException = return . Left . show

  decodeTasks:: ByteString-> Either String (Vector Task)
  decodeTasks = fmap snd . Cassava.decodeByName

  decodeTasksFromFile:: IO (Either String (Vector Task))
  decodeTasksFromFile = catchShowIO (ByteString.readFile filePath) >>= return . either Left decodeTasks

  encodeTasks::Vector Task -> ByteString
  encodeTasks = Cassava.encodeDefaultOrderedByName.toList

  encodeTaskToFile:: Vector Task-> IO (Either String ())
  encodeTaskToFile = catchShowIO . (ByteString.writeFile filePath) . encodeTasks

  actOnCrComm::Task-> IO (Either String ())
  actOnCrComm tt = do 
    res <- decodeTasksFromFile
    case res of
      Left _ -> encodeTaskToFile . fromList $ [tt] 
      Right tasks -> let
        lts =  toList tasks
        in encodeTaskToFile . fromList $ lts ++ [tt] 
        
  showGivenTask::Vector Task -> IO()
  showGivenTask vT = do
    print taskHeader
    aug 1 . toList $ vT
    where
      aug:: Int -> [Task] -> IO()
      aug _ [] = return ()
      aug count (x:xs) = do
        putStr "No."
        putStr . show $ count
        print x
        aug (count+1) xs

  actOnShowAll::IO (Either String ())
  actOnShowAll = do
    res <- decodeTasksFromFile
    case res of
      Left ms -> return . Left $ ms 
      Right tasks -> do
        showGivenTask tasks
        return . Right $ ()

  creaShoCri::Maybe a-> (a->a->Bool)-> (a->Bool)
  creaShoCri mA toC = case mA of
    Nothing -> const True
    Just a -> toC a

  actOnShow::ParTask-> IO (Either String ())
  actOnShow pt = do
    let markCri = creaShoCri (getPMark pt) (==)
    let descCri = creaShoCri (getPDesc pt) (isInfixOf)
    let ddCri = creaShoCri (getPDD pt) (==)
    let rmCri = creaShoCri (getPRe pt) (==)
    res <- decodeTasksFromFile
    case res of
      Left ms -> return . Left $ ms 
      Right tas -> do
        let remain = Vector.filter(rmCri . getReminder). Vector.filter(ddCri . getDueDate). Vector.filter(descCri . getDesc). Vector.filter(markCri . getMark) $ tas
        showGivenTask remain
        return . Right $ ()

  actOnDel::ParTask-> IO (Either String ())
  actOnDel pt = do
    let markCri = creaShoCri (getPMark pt) (/=)
    let descCri = creaShoCri (getPDesc pt) (\x y->not$isInfixOf x y)
    let ddCri = creaShoCri (getPDD pt) (/=)
    let rmCri = creaShoCri (getPRe pt) (/=)
    res <- decodeTasksFromFile
    case res of
      Left ms -> return . Left $ ms 
      Right tas -> do
        let remain = Vector.filter(rmCri . getReminder). Vector.filter(ddCri . getDueDate). Vector.filter(descCri . getDesc). Vector.filter(markCri . getMark) $ tas
        encodeTaskToFile remain