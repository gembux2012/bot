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

module Network.Api
  ( 
  )
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
import Data.Aeson.Lens (AsPrimitive, AsValue, _Array, _Integer, _JSON, _Object, _String, key, values, values)
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
import Data.Aeson (decodeStrict,parseJSON)
import Network.Types
import Network.Types( ResponseMessage(..),MessageObject(..))

--import Network.Types (MessageVK)



--import Control.Monad.Base (fromList)

--https://api.vk.com/method/groups.getLongPollServer?access_token=&group_id=202652768&v=5.130
-- send https://api.vk.com/method/messages.send?user_id=454751226&message=&title=gh&access_token=v=5.50
-- outh https://api.vk.com/method/groups.getLongPollServer?access_token=57f5906e918c8dc83168e8d92770dde7610f53d0b143eb2030bc2f116532e07a463d0cdd82db544fc3efb&group_id=202652768&v=5.50
-- receive https://lp.vk.com/wh202652768?act=a_check&key=f01f9b9c85df0878d0e181a28ee14b3909f06099&ts=10&wait=25
idGroup = "202652768" :: BS8.ByteString

keyGroup = "13b47b20e6324c0dcc288baec4a318ee359bd16dc2f57c7bf215755241faf955de12b014a780ac7f9e455"

keyUser = "454751226"

versionService = "5.50" :: BS8.ByteString

uriVK = "api.vk.com/method/"

newtype SecKey = SecKey { secKey :: String }
data Url= Url
 { requestHost :: BS8.ByteString,
   requestMethod :: BS8.ByteString,
   requestPath :: BS8.ByteString,
   requestQS :: Query 
 } 

getKeyAccessUrl  = Url 
 {requestHost = uriVK,
  requestMethod = "GET",
  requestPath = "groups.getLongPollServer" ,
  requestQS = [("access_token", Just keyGroup),("group_id", Just idGroup),("v", Just versionService)]
 }
 
getMessageUrl k ts = Url
 {requestHost = "lp.vk.com",
   requestMethod = "GET",
   requestPath = "wh" <> idGroup ,
   requestQS = [("act", Just "a_check" ),("key", Just k),("ts", Just ts),("wait", Just 25)]
  } 
{--
requestVK' ::
  MonadThrow m =>
  MonadIO m =>
  --MonadBase IO m =>
  String ->
   m ResponseMessage
   --}
requestVK :: String -> Url -> m () 
requestVK method Url{..} = do
  let request
        = setRequestHost requestHost  
        $ setRequestMethod requestMethod
        $ setRequestPath requestPath
        $ setRequestQueryString requestQS
        $ setRequestSecure True
        $ setRequestPort 443
        $ defaultRequest
  response <- httpBS request
  
  --logI $ T.pack url
  let status = getResponseStatusCode response
  case status of
    200 -> requestVK $ prependRequest method (getResponseBody response)
    _ -> logE $ T.pack  ("method " <> show request <> " status " <> show status <> " app will be stopped")
          

prependRequest m b 
    | m == "getKeyAccess" = getKeyAccess b
    | m == "getMessageVK" = getMessageVK b


getKeyAccess body  = 
  case body ^? key "error" . key "error_msg" . _String of
    Just error -> logE $ "authorisation error " <>   error <> " app will be stopped"
    Nothing ->  
      case extractSecKey body of
        Nothing  ->  logE  " unknown error, bot stop"
        Just (sk, ts) -> "getMessageVk" (getMessageUrl   (BS8.pack  sk)  (BS8.pack $ show ts))
 

getMessageVK body = 
      case body ^? key "failed" . _Integer of
        Just failed -> do
          case failed of
            1 -> do
              case body ^? key "ts" . _Integer of
                Nothing ->  logE " unknown error, bot stop"
                Just ts -> getMessageVK secKey (show ts')
            _ -> authVK
        Nothing -> do 
          
          case  decodeStrict body' :: Maybe MessageVK of
            Just mess  -> pure $ MessageVk mess 
            Nothing -> pure $ Error  " unknown error, bot stop"
             {--
             case updates of
              [] -> do  
                logI "not message"    
                logI "awaiting message"
                getMessageVK secKey (show ts)
              [msg] -> do
               logI $ "message received: " <>  T.pack(text (_object msg))
               sendMessage secKey ts msg 
             --}
  where
    url =
      "https://lp.vk.com/wh" ++ idGroup ++ "?act=a_check&key=" ++ secKey
        ++ "&ts="
        ++ ts
        ++ "&wait=25"
   
    
extractSecKey :: AsValue s => s -> Maybe (String, Integer)
extractSecKey body =
  body ^? key "response" . key "key" . _String >>= \secKey ->
  body ^? key "response" . key "ts" . _Integer >>= \ts -> Just (T.unpack secKey, ts)

extractMessage :: AsValue s => s -> Maybe (Integer, Text)
extractMessage body =
  body ^? key "updates" . key "object" . key "from_id" . _Integer >>= \from_id ->
  body ^? key "updates" . key "object" . key "text" . _String >>= \msg -> Just (from_id, msg)

sendMessage secKey ts msg = do
 body <- requestVK' url
 case body of
   Message body' ->
     case body' ^? key "error" . key "error_msg" . _String of
       Just err -> pure $ Error ("error sending message " <> T.unpack err <> " app will be stopped")
       Nothing -> do
           logI $ "sending message: " <> T.pack(text (_object msg))
           logI "awaiting message"
           getMessageVK secKey (show ts) 
 where
    url = "https://api.vk.com/method/messages.send?user_id=" <>  show(from_id(_object msg)) 
           <> "&message=" <> text (_object msg)  <>   "&title=gh&access_token="  <>  keyGroup <> "&v=5.50"