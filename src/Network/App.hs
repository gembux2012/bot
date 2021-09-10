{-# LANGUAGE AllowAmbiguousTypes #-}
--{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Network.App
  
where



import qualified Control.Concurrent.Lifted as CCL
--import qualified Data.Aeson.Lens as L

import Control.Lens (_Wrapped)
import Control.Lens.Combinators (preview)
import Control.Lens.Fold ((^?))
import qualified Control.Monad as CM
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
--import Data.Aeson
import qualified Data.Aeson.Lens as AL -- ( AsPrimitive, AsValue, _Array, _Integer, _JSON, _Object, _String, key, values, values, values )
import Data.Aeson.Lens (values)
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Char8 (pack)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as TIO
import GHC.Generics
import Logger.Class (Log (..))
import Network.HTTP.Conduit (http)
import Network.HTTP.Simple
import Data.Aeson.Types (Parser, parseMaybe, FromJSON, Value, withObject, (.:), fieldLabelModifier, genericParseJSON, defaultOptions)
import Data.Aeson (decodeStrict,parseJSON, eitherDecodeStrict, encode)
--import Data.Aeson.Encode
import Data.Either 
import Data.String
--import Control.Monad.Error
import Network.Types 
--import Network.ErrorTypes
import Data.Maybe 
-- import Network.ErrorTypes (ErrorVK)
import Network.Types 
--import Network.Types (Message (..))
import Network.URI
import Data.Text (pack)
import Control.Monad (when )
import Control.Monad.Catch 
import Config (Config)
import qualified Data.ByteString.Lazy as LBS


idGroup = "202652768" :: BS8.ByteString

keyGroup = "13b47b20e6324c0dcc288baec4a318ee359bd16dc2f57c7bf215755241faf955de12b014a780ac7f9e455"

keyUser = "454751226"

versionService = "5.103" :: BS8.ByteString

uriVK = "api.vk.com"

newtype SecKey = SecKey { secKey :: String }

 

getKeyAccessUrl  = Url 
 { -- requestHost = uriVK,
  requestMethod = "GET",
  requestPath = "https://" <> uriVK <> "/method/groups.getLongPollServer" ,
  requestQS = [("access_token", Just keyGroup),("group_id", Just idGroup),("v", Just versionService)]
 }

getMessageUrl :: BS8.ByteString -> BS8.ByteString  -> BS8.ByteString -> Url
getMessageUrl k server  ts = Url
 { -- requestHost = BS8.pack   server     ,
   requestMethod = "GET",
   requestPath =  server ,
   requestQS = [("act", Just "a_check" ),("key", Just k),("ts", Just ts),("wait", Just "25")]
  } 

sendMessageUrl :: BS8.ByteString ->  BS8.ByteString -> BS8.ByteString -> Url   
sendMessageUrl  user_id message buttons = Url
 {
   requestMethod = "GET",
   requestPath = "https://" <> uriVK <> "/method/messages.send" ,
   requestQS = [("user_id", Just user_id ),
               ("random_id", Just "0"),
               ("peer_id", Just idGroup),
               ("message", Just message),
               ("dont_parse_links", Just "0"),
               ("disable_mentions", Just "0"),
               ("intent", Just "default"),
              -- ("title", Just ""),
               ("keyboard" , Just buttons),
               ("access_token", Just keyGroup),("v", Just versionService)]
 } 

repeatButtons = Buttons 
 { one_time = False,
   buttons = 
   [
    [Button
     { action = Action
      { type' = "text",
        payload = "{\"button\": \"1\"}",
        label = "1"
      },
      color = "primary"
     },

    Button
        { action = Action 
         { type' = "text",
           payload = "{\"button\": \"2\"}",
           label = "2" 
         },
         color = "primary"
        },
    Button
        { action = Action 
         { type' = "text",
           payload = "{\"button\": \"3\"}",
           label = "3" 
         },
         color = "primary"
        }    
   ]
  ]

 }

emptyButtons = Buttons
  { one_time = False,
    buttons =  [ ]
    }


requestVK ::
 MonadIO m =>
 MonadThrow m =>
 Config -> Url ->  m Message
requestVK  cong Url{..}  = do
  request' <- parseRequest $ BS8.unpack  requestPath
  let request
        =  setRequestMethod requestMethod
        $ setRequestQueryString requestQS
         request'
  response <- httpBS request
  let status = getResponseStatusCode response
  case status of
    200 -> do 
     let body = getResponseBody response
     case decodeStrict.getResponseBody $ response :: Maybe Message of
      Just  mess -> do 
             pure mess
      Nothing -> do
        pure $   ErrorVK $ ErrVK "0" "Invalid Json"
    _ -> do 
        pure $ ErrorVK $ ErrVK "1" "no 200"
       
dispatcherAnswer :: String -> String ->  ( BS8.ByteString,  BS8.ByteString)
dispatcherAnswer ['\\','r','e','p','e','a','t'] _ = (BS8.pack "how many times will you repeat ?",  
                                                   LBS.toStrict $ encode repeatButtons)
dispatcherAnswer str  btn 
  |  btn /= "" =  (BS8.pack $ " ОК, i will repeat " <> btn <> " time(s), try",
                     LBS.toStrict $ encode emptyButtons)
  | otherwise = ( encodeUtf8.T.pack $ str, LBS.toStrict $ encode emptyButtons)
        