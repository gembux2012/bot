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
import Data.Aeson (decodeStrict,parseJSON, eitherDecodeStrict)
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
import Control.Monad (when )
import Control.Monad.Catch 

--import Network.Types (MessageVK)



--import Control.Monad.Base (fromList)

--https://api.vk.com/method/groups.getLongPollServer?access_token=&group_id=202652768&v=5.130
-- send https://api.vk.com/method/messages.send?user_id=454751226&message=&title=gh&access_token=v=5.50
--https://vk.com/dev/messages.send?params[user_id]=454751226&params[random_id]=0&params[message]=Test%20message&params[dont_parse_links]=0&params[disable_mentions]=0&params[intent]=default&params[v]=5.130
-- outh https://api.vk.com/method/groups.getLongPollServer?access_token=57f5906e918c8dc83168e8d92770dde7610f53d0b143eb2030bc2f116532e07a463d0cdd82db544fc3efb&group_id=202652768&v=5.50
-- receive https://lp.vk.com/wh202652768?act=a_check&key=f01f9b9c85df0878d0e181a28ee14b3909f06099&ts=10&wait=25
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
 Monad m =>
 MonadThrow m =>
 Applicative m => 
 --Log m =>
 --Log IO =>
 Url -> m Message



requestVK  Url{..} = do
  --logI $ T.pack.show $ Url{..}
  request' <- parseRequest $ BS8.unpack $ requestPath
  let request
        =  setRequestMethod requestMethod
        $ setRequestQueryString requestQS
         request'
  --logI $ T.pack.show $ request
  response <- httpBS request
 
  let status = getResponseStatusCode response
  case status of
    200 -> do 
     let body = getResponseBody response
     --logI $ T.pack.BS8.unpack $ body
     case decodeStrict.getResponseBody $ response :: Maybe Message of
      Just  mess -> do 
        -- logI $ T.pack.show $ mess
        pure mess
      Nothing -> do
        --logI $ T.pack "Invalid Json"  
        --logI $ T.pack.BS8.unpack $ body
        pure $   ErrorVK $ ErrVK "0" "Invalid Json"
    _ -> do 
        --logI $ T.pack "no 200"
        pure $ ErrorVK $ ErrVK "1" "no 200"
       
        