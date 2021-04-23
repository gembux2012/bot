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
import qualified Data.Text as T
import Network.HTTP.Simple (Query)
import Control.Monad.Cont (when)
import qualified Data.Aeson.Lens as AL 
import Control.Lens.Fold
import Data.Text.Encoding (encodeUtf8)


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
   | Failed {failed :: Failed'}
             
   | NoMessage
  deriving ( Show) 


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
 
instance FromJSON Message where
  parseJSON = withObject "message or response or access or filed" $ \o ->  asum [
        Message' <$> o .: "ts" <*> o .: "updates",
        Response <$> o .: "response",
        --Access   <$>  o .:  "response" >>= \r -> (r .: "key"),
        Just (getAccess o) -> \acc -> acc,
        Failed  <$> o .: "failed"  
         ]
         
getAccess  o =
  o ^? AL.key "response" . AL.key "key" . AL._String >>= \secKey ->
  o ^? AL.key "response" . AL.key "server" . AL._String >>= \server ->
  o ^? AL.key "response" . AL.key "ts" . AL._Integer >>= \ts -> 
  Just (Access'{ key = T.unpack secKey , server = T.unpack server, ts = T.unpack.show $ ts})                        

instance FromJSON Failed' where
  parseJSON = withObject " failed " $ \o -> do
    failed' <- o .: "failed"
    ts'' <-   o .:? "ts"
    return Failed' {..}
              

instance FromJSON Access' where
  parseJSON = withObject " access " $ \o -> do
   resp <- o .: "response"
   key <- resp .: "key"
   server <- resp .: "server"
   ts <- resp .: "ts"
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


