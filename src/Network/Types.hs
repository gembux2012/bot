{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Types
 
where

import GHC.Generics
import qualified Data.ByteString.Char8 as BS8
import Data.Aeson
import Data.Foldable


import Data.Aeson.Types (FromJSON)
import Data.Text.Internal.Lazy (Text)
import Network.HTTP.Simple (Query)
import Control.Monad.Cont (when)


--data ResponseMessage' = Message' BS8.ByteString | Stop

data Method = GetKeyAccess | GetMessage | SendMessage
 deriving( Eq)

data Url= Url
 { requestHost :: BS8.ByteString,
   requestMethod :: BS8.ByteString,
   requestPath :: BS8.ByteString,
   requestQS :: Query 
 } 




data Button = DataButton
 {

 }
 deriving (Generic, FromJSON, Show)
 
data Message = Message'
 { ts' :: String ,
   updates :: [MessageUpdates]
 } | Response{ response :: Integer }
   | Access {response' :: Access'}
   | Failed {failed :: Int,
             ts'' ::  Int
             }
   | NoMessage
  deriving (Generic, Show) 


data Access' = Access'     
  {   
   key :: String,
   server :: String,
   ts :: Int
   }
   deriving ( Show) 

instance FromJSON Message where
  parseJSON = withObject "message or access or filed" $ \o ->  asum [
        Message' <$> o .: "ts" <*> o .: "updates",
        Response <$> o .: "response",
        o .: "failed" >>= \ofail -> 
         when (ofail == 1)  Failed <$> o .: "failed" <*> o .: "ts"
         Failed  <$> o .: "failed" <*> null Int 
         ]
instance FromJSON Access' where
  parseJSON = withObject " access " $ \o ->          
   o .: "response" >>= \orsp -> Access' <$> orsp .: "key" <*> "server" <*> "ts" 
 

  
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

data MessageObject = MessageObject
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


