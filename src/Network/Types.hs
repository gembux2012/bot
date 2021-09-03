{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Types
 
where

import GHC.Generics
import qualified Data.ByteString.Char8 as BS8
import Data.Aeson
import Data.Foldable


import Data.Aeson.Types (FromJSON)
import qualified Data.Text as T
import Network.HTTP.Simple (Query)
import Control.Monad.Cont (when)
import qualified Data.Aeson.Lens as AL 
import Control.Lens.Fold
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Simple
import Text.Read (readMaybe)


--data ResponseMessage' = Message' BS8.ByteString | Stop

data Method = GetKeyAccess | GetMessage | SendMessage
 deriving( Eq)

data Url= Url
 { --requestHost :: BS8.ByteString,
   requestMethod :: BS8.ByteString,
   requestPath :: BS8.ByteString,
   requestQS :: Query 
 }
 deriving (Eq,Show)

data Url'= Url'
 { requestPath' :: BS8.ByteString,
   queryStr :: Query
 }


data Buttons = Buttons
 {one_time :: Bool,
  buttons :: [[Button]]
 }
 deriving (Generic, ToJSON)
 

 
data Button = Button
 { action :: Action,
   color :: String
 }
 deriving (Generic, ToJSON)
 
data Action = Action
  { type' :: String,
    payload :: String,
    label :: String
  } 
 

instance ToJSON Action where
  toJSON Action{..} = object [
    "type" .= type',
    "payload"  .= payload,
    "label" .= label 
    ]  


data ToRequest = ACCESS Url | GETMESSAGE Url  | SENDMESSAGE Url
 


data Message  = Message' 
 { ts' :: String,
   updates :: [MessageUpdates]
   
 } | Response{ response :: Integer  }
   | Access  Access'  
   | Failed {failed :: Failed'}
   | ErrorVK {error :: ErrVK}          
   | NoMessage
   
  deriving ( Show) 

data ErrVK= ErrVK 
 { error_code :: String,
   error_msg :: String 
 } 
 deriving (Generic, FromJSON,  Show)
 
data Access' = Access'     
  {   
   key :: String,
   server :: String,
   ts :: String
   }
   deriving ( Show) 

data Failed' = Failed'
 {failed' :: Int,
  ts'' :: Maybe Int
  }  
 deriving ( Show)
 

instance FromJSON Message   where
  parseJSON  = withObject "message or response or access or filed" $ \o  ->  asum [
        Access   <$>  o .:  "response" ,
        Message' <$> o .: "ts" <*> o .: "updates" , 
        Response <$> o .: "response"  ,
        Failed  <$> o .: "failed"  
         ]
         
                    

instance FromJSON Failed' where
  parseJSON = withObject " failed " $ \o -> do
    failed' <- o .: "failed"
    ts'' <-   o .:? "ts"
    return Failed' {..}
              

instance FromJSON Access' where
  parseJSON = withObject " access " $ \o -> do
   key <- o .: "key"
   server <- o .: "server"
   ts <-  o .: "ts"
   return Access'{..}
             
 
 

{-  
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
-}
data MessageUpdates = MessageUpdates
  { _type :: String,
    from_id :: Integer,
    text :: String,
    _payload :: String
  }
  deriving (Generic, Show)
  
instance FromJSON MessageUpdates where
   parseJSON = withObject " message " $ \o -> do
     _type <- o .: "type" 
     obj <- o .: "object" 
     case _type of
      "message_new" -> do
        message <- obj .: "message"  
        from_id <- message .: "from_id"
        text <- message .: "text"
        _payload <- do 
         pl <-  message .:? "payload"
         case  pl :: Maybe String of
           Just _ -> return text     
           Nothing -> return ""
           
        --let _payload = ""
        return MessageUpdates{..}
      _ -> do 
        let from_id = 0
        let text = ""
        let _payload = ""
        return MessageUpdates{..}  
  

     
     
{--
data MessageObject = MessageObject
 { id :: Int,
   from_id :: Maybe Integer,
   owner_id :: Maybe Integer,
   date :: Maybe Integer,
   marked_as_ads :: Maybe Integer,
   post_type :: Maybe String,
   text :: Maybe String,
   can_edit :: Maybe Int,
   created_by :: Maybe Integer,
   can_delete :: Maybe Int,
   comments :: Maybe MessageComment,
   out  :: Maybe Int,
   user_id  :: Maybe Integer,
   read_state  :: Maybe Int,
   title  :: Maybe String,
   body  :: Maybe String 
   -- random_id  :: Int
  --owner_ids  :: []
  }
  deriving (Generic, FromJSON, Show)
--}
data MessageComment = MessageComment
  { count :: Int
  }
  deriving (Generic, FromJSON, Show)


