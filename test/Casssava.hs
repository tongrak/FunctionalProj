{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OpenData
    -- ( 
    --     Item(..)
    -- , ItemType(..)
    -- , decodeItems
    -- , decodeItemsFromFile
    -- , encodeItems
    -- , encodeItemsToFile
    -- , filterCountryItems
    -- , itemHeader
    -- , japanItem
    -- , japanRecord
    -- )
  where

    import Control.Exception (IOException)
    import qualified Control.Exception as Exception
    import qualified Data.Foldable as Foldable
    
    import Data.Csv( DefaultOrdered(headerOrder)
        , FromField(parseField)
        , FromNamedRecord(parseNamedRecord)
        , Header
        , ToField(toField)
        , ToNamedRecord(toNamedRecord)
        , (.:)
        , (.=)
        )
    import qualified Data.Csv as Cassava

    import Data.Text (Text)
    import qualified Data.Text.Encoding as TextEn

    import Data.Vector (Vector)
    import qualified Data.Vector as Vector

    import Data.ByteString.Lazy (ByteString)
    import qualified Data.ByteString.Lazy as ByteString

    data Item = Item { 
        itemName :: Text
        , itemLink :: Text
        , itemType :: ItemType
        }
        deriving(Eq, Show)

    data ItemType = Country | Other Text
        deriving (Eq, Show)

    japanRecord :: ByteString
    japanRecord = "Japan,http://www.data.go.jp/,International Country"

    japanItem :: Item
    japanItem = Item { 
        itemName = "Japan"
        ,itemLink = "http://www.data.go.jp/"
        ,itemType = Country
        }

    instance FromNamedRecord Item where
        parseNamedRecord m = Item <$> m .: "Item" 
            <*> m .: "Link" 
            <*> m .: "Type" 

    instance FromField ItemType where
        parseField "International Country" = pure Country
        parseField otherType = Other <$> Cassava.parseField otherType

    -- simFunc = Cassava.decodeByName "Item,Link,Type\n\\Japan,http://www.data.go.jp/,International Country\n"
    --     :: Either String (Header, Vector Item)

    decodeItem::ByteString -> Either String (Vector Item)
    decodeItem = fmap snd .  Cassava.decodeByName

    decodeItemsFromFile:: FilePath -> IO (Either String (Vector Item))
    decodeItemsFromFile fileP = catchShowIO (ByteString.readFile fileP) >>= return . either Left decodeItem

    catchShowIO:: IO a -> IO (Either String a)
    catchShowIO action = (fmap Right action) `Exception.catch` handleIOExcep
        where
            handleIOExcep:: IOException -> IO (Either String a)
            handleIOExcep = return . Left . show

    filterCountryItems:: Vector Item -> Vector Item
    filterCountryItems = Vector.filter isCountryItem
        
    isCountryItem::Item -> Bool
    isCountryItem = (Country  ==) . itemType

    

-- main:: IO()
