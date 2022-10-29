-- extensions
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CommEnforcer (
  actOnCrComm
) where
  -- Vector
  import Data.Vector (Vector, fromList)
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
  -- local
  import TaskModel(Task)

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