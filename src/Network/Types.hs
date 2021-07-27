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



data Button = DataButton
 {

 }
 deriving (Generic, FromJSON, Show)

data ToRequest = ACCESS Url | GETMESSAGE Url  | SENDMESSAGE Url
 


data Message  = Message' 
 { ts' :: String,
   updates :: [MessageUpdates]
   
 } | Response{ response :: Integer  }
   | Access  Access'  
   | Failed {failed :: Failed'}
   | ErrorVK {error :: Err}          
   | NoMessage
   
  deriving ( Show) 

data Err= Err 
 { error_code :: Int,
   error_msg :: String 
 } 
 deriving (Generic, FromJSON,  Show)
 
data Access' = Access'     
  {   
   key :: String,
   server :: String,
   ts :: Int
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
   -- resp <- o .: "response"
   key <- o .: "key"
   server <- o .: "server"
   ts <-  o .: "ts"
   return Access'{..}
             
   -- o .: "response" >>= \orsp -> Access' <$> orsp .: "key" <*> "server" <*> "ts" 
 

  
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

data MessageComment = MessageComment
  { count :: Int
  }
  deriving (Generic, FromJSON, Show)


