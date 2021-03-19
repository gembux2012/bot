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
  ( run,
  )
where

-- import Data.Text.Internal.Lazy (Text)
-- .Types (ToJSON, FromJSON)

-- import Data.Aeson.Encode (encode)

--import Control.Exception.Base (try)

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



--import Control.Monad.Base (fromList)

--https://api.vk.com/method/groups.getLongPollServer?access_token=&group_id=202652768&v=5.130
-- send https://api.vk.com/method/messages.send?user_id=454751226&message=&title=gh&access_token=v=5.50
-- outh https://api.vk.com/method/groups.getLongPollServer?access_token=57f5906e918c8dc83168e8d92770dde7610f53d0b143eb2030bc2f116532e07a463d0cdd82db544fc3efb&group_id=202652768&v=5.50
-- receive https://lp.vk.com/wh202652768?act=a_check&key=f01f9b9c85df0878d0e181a28ee14b3909f06099&ts=10&wait=25
idGroup = "202652768" :: String

keyGroup = "13b47b20e6324c0dcc288baec4a318ee359bd16dc2f57c7bf215755241faf955de12b014a780ac7f9e455"

keyUser = "454751226"

versionService = "5.50" :: String

uriVK = "api.vk.com"

 
run ::
  MonadThrow m =>
  MonadIO m =>
  MonadBase IO m =>
  Log m =>
  m ()
run = do
  mes <- authVK
  case mes of
    Message mes -> logI $ T.pack (BS8.unpack mes)
    Stop -> logI "bot stop "

requestVK' ::
  MonadThrow m =>
  MonadIO m =>
  MonadBase IO m =>
  Log m =>
  String ->
  m ResponseMessage
requestVK' url = do
  request <- parseRequest url
  response <- httpBS request
  --logI $ T.pack url
  let status = getResponseStatusCode response
  case status of
    200 -> pure $ Message (getResponseBody response)
    _ -> do 
     logE $ "method " <> T.pack (show request) <> " status " <> T.pack (show status) <> " app will be stopped"
     pure Stop 

authVK ::
  MonadThrow m =>
  MonadIO m =>
  MonadBase IO m =>
  Log m =>
  m ResponseMessage
authVK = do
  body <- requestVK' url
  case body of
    Message body' ->
      case body' ^? key "error" . key "error_msg" . _String of
        Just err -> do
         logE $ "authorisation Error " <> err <> " app will be stopped"
         pure Stop
        Nothing -> do
          logI "authorization successful, awaiting message"
          case extractSecKey body' of
            Nothing  ->  Stop <$ logE " unknown error, bot stop"
            Just (secKey, ts) -> getMessageVK secKey (show ts)
  where
    url =
      "https://api.vk.com/method/groups.getLongPollServer?access_token=" ++ keyGroup
        ++ "&group_id="
        ++ idGroup
        ++ "&v="
        ++ versionService

getMessageVK ::
  MonadThrow m =>
  MonadIO m =>
  MonadBase IO m =>
  Log m =>
  String ->
  String ->
  m ResponseMessage
getMessageVK secKey ts = do
  body <- requestVK' url
  case body of
    Stop -> return Stop
    Message body' ->
      case body' ^? key "failed" . _Integer of
        Just failed -> do
          case failed of
            1 -> do
              case body' ^? key "ts" . _Integer of
                Nothing ->  Stop <$ logE " unknown error, bot stop"
                Just ts' -> getMessageVK secKey (show ts')
            _ -> authVK
        Nothing -> do 
          
          case  decodeStrict body' :: Maybe DataMessage of
            Just DataMessage{..}  -> 
             case updates of
              [] -> do  
                logI "not message"    
                logI "awaiting message"
                getMessageVK secKey (show ts)
              [msg] -> do
               logI $ "message received: " <>  T.pack(text (_object msg))
               sendMessage secKey ts msg 
            Nothing -> Stop <$ logE " unknown error, bot stop" 
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
       Just err -> do
        logE $ "error sending message " <> err <> " app will be stopped"
        pure Stop
       Nothing -> do
           logI $ "sending message: " <> T.pack(text (_object msg))
           logI "awaiting message"
           getMessageVK secKey (show ts) 
 where
    url = "https://api.vk.com/method/messages.send?user_id=" <>  show(from_id(_object msg)) 
           <> "&message=" <> text (_object msg)  <>   "&title=gh&access_token="  <>  keyGroup <> "&v=5.50"