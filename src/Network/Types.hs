{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Network.Types
 
where

import GHC.Generics
import qualified Data.ByteString.Char8 as BS8
import Data.Aeson


import Data.Aeson.Types (FromJSON)
import Data.Text.Internal.Lazy (Text)
import Network.HTTP.Simple (Query)


data Method = GetKeyAccess | GetMessage 
 deriving (Eq)

data Url= Url
 { requestHost :: BS8.ByteString,
   requestMethod :: BS8.ByteString,
   requestPath :: BS8.ByteString,
   requestQS :: Query 
 } 

data ResponseMessage
  = NoResponse | Message BS8.ByteString | Error String | Auth ( BS8.ByteString,  BS8.ByteString) | MessageVk MessageVK


data Button = DataButton
 {

 }
 deriving (Generic, FromJSON, Show)


data MessageVK = MessageVK
 { ts :: String,
   updates :: [MessageUpdates]
 }
 deriving (Generic, FromJSON, Show)
  
data MessageUpdates = MessageUpdates
  { _type :: String,
    _object :: MessageObject,
    _group_id :: Integer,
    _event_id :: String
  }
  deriving (Generic, Show)


instance FromJSON MessageUpdates where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }

data MessageObject = ObjectWall
 { id :: Int,
   from_id :: Integer,
   owner_id :: Integer,
   date :: Integer,
   marked_as_ads :: Integer,
   post_type :: String,
   text :: String,
   can_edit :: Int,
   created_by :: Integer,
   can_delete :: Int,
   comments :: MessageComment
 }  | ObjectUser
 {

 }
  deriving (Generic, FromJSON, Show)

data MessageComment = MessageComment
  { count :: Int
  }
  deriving (Generic, FromJSON, Show)


