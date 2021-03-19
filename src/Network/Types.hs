{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Network.Types where

import GHC.Generics
import qualified Data.ByteString.Char8 as BS8
import Data.Aeson


import Data.Aeson.Types (FromJSON)


data ResponseMessage = Message BS8.ByteString | Stop

data Button = DataButton
 {

 }
data MessageVK = MessageVK
 { ts :: String,
   updates :: [MessageUpdates]
  }
 deriving (Generic, FromJSON, Show)



  
data MessageUpdates = MessageUpdates
  { _type :: String,
    _object :: Value,
    _group_id :: Integer,
    _event_id :: String
  }
  deriving (Generic, Show)
instance FromJSON MessageUpdates where
    parseJSON = withObject "m_updates" $ \o -> do
     _type <- o .: "type"
     when( "type" == "wall_post_new") _object <- 
     

instance FromJSON MessageUpdates where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }

data MessageObjectWall = MessageObject
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
 }
  deriving (Generic, FromJSON, Show)

data MessageComment = MessageComment
  { count :: Int
  }
  deriving (Generic, FromJSON, Show)


