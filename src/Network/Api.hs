{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}

module Network.Api
  ( run,
  )
where

-- import Data.Text.Internal.Lazy (Text)
-- .Types (ToJSON, FromJSON)

-- import Data.Aeson.Encode (encode)

--import Control.Exception.Base (try)

import qualified Control.Concurrent.Lifted as CCL
import           Control.Lens.Fold ((^?))
import           Control.Lens.Combinators (preview)
import qualified Control.Monad as CM
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson
--import qualified Data.Aeson.Lens as L
import Data.Aeson.Lens ( _String, key, AsValue, AsPrimitive, values , _Integer)
import           Data.Aeson.Lens (values)
import qualified Data.ByteString.Char8 as BS8
import           Data.ByteString.Char8 (pack)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as TIO
import           GHC.Generics
import           Logger.Class (Log (..))
import           Network.HTTP.Conduit (http)
import           Network.HTTP.Simple (HttpException, JSONException, getResponseBody, getResponseHeader, getResponseStatusCode, httpBS, httpJSON, httpJSONEither, parseRequest)
import           Network.HTTP.Simple

--https://api.vk.com/method/groups.getLongPollServer?access_token=&group_id=202652768&v=5.130
-- send https://api.vk.com/method/messages.send?user_id=454751226&message=&title=gh&access_token=v=5.50
-- outh https://api.vk.com/method/groups.getLongPollServer?access_token=57f5906e918c8dc83168e8d92770dde7610f53d0b143eb2030bc2f116532e07a463d0cdd82db544fc3efb&group_id=202652768&v=5.50
-- receive https://lp.vk.com/wh202652768?act=a_check&key=f01f9b9c85df0878d0e181a28ee14b3909f06099&ts=10&wait=25
idGroup = "202652768"

keyGroup = "13b47b20e6324c0dcc288baec4a318ee359bd16dc2f57c7bf215755241faf955de12b014a780ac7f9e455"

keyUser = "454751226"

versionService = "5.50"

uriVK = "api.vk.com"

-- {"response":{"key":"e2d4c232a80a977a0211d2f0093767032bbd5635","server":"https:\/\/lp.vk.com\/wh202652768","ts":10}}
data RequestVK = RequestVK
  { requestHost :: BS8.ByteString,
    requestPath :: BS8.ByteString,
    requestMethod :: BS8.ByteString,
    requestQuery :: Query
  }

authRequestVk :: RequestVK
authRequestVk = RequestVK
 { requestHost = BS8.pack uriVK,
   requestPath = "/method/",
   requestMethod = "groups.getLongPollServer",
   requestQuery = [("access_token", Just keyGroup), ("group_id", Just idGroup),("v", Just versionService)]
  }
 



requestVK ::
  MonadThrow m =>
  MonadIO m =>
 -- MonadBase IO m =>
  Log m =>
  RequestVK ->
  m BS8.ByteString

requestVK RequestVK{..} = do
  let request 
        = setRequestMethod "GET"
        $ setRequestHost requestHost
        $ setRequestPort 443
        $ setRequestSecure True
        $ setRequestPath (requestPath <>  requestMethod)
        $ setRequestQueryString  requestQuery
        defaultRequest
  
  response <- httpBS request
  let status = getResponseStatusCode response
  case status of
    200 -> do
      case requestMethod of
        "groups.getLongPollServer" -> do
          let body = getResponseBody response 
          case body ^? key "error" . key "error_msg" . _String of
            Just err -> do
              logE $ "method " <> T.pack (show requestMethod) <> " return " <> T.pack (show err) <> " app will be stopped"
              return "/stop"
            Nothing -> do
                  logI $ "method " <> T.pack (show requestMethod) <> " status " <> T.pack (show status)
                  case extractMessage body of
                   Nothing -> return "/stop unknown"
                   Just( secKey, ts) ->
                    requestVK RequestVK   
                           { requestHost = "lp.vk.com",
                              requestPath = "/wh" <> idGroup,
                              requestMethod = "", 
                              requestQuery = [("act", Just "a_check"),("key",  Just secKey ), ("ts", Just ts),("wait", Just "25")]
                             }        
        "" -> do 
           logI $  "method  chek" 
           return "mesage "                    
                

        
    _ -> do
      logE $ "method " <> T.pack (show request) <> " status " <> T.pack (show status) <> " app will be stopped"
      return "/stop "






extractMessage :: AsValue s => s -> Maybe (BS8.ByteString, BS8.ByteString)
extractMessage body  = 
       body ^? key "response" . key "key" . _String >>= \secKey ->
       body ^? key "response" . key "ts" . _Integer >>= \ts -> Just (encodeUtf8 secKey  ,BS8.pack $ show ts)



run ::
  MonadThrow m =>
  MonadIO m =>
  MonadBase IO m =>
  Log m =>
   m ()
run  = do 
                  mes <- requestVK  authRequestVk
                  logI $  T.pack (BS8.unpack mes)
                  
                  

--BS8.putStrLn body
--putStrLn auth
--response <- try $ httpBS  request
{--
   --let key = getResponseBody response ^.  "keyAuth" . L._String

   --let server = (getResponseBody response) ^? key "response". key "key" . _String
   let server = findInJsonFromKey (getResponseBody response)  ["response", "key"]
      --let ts = getResponseBody response ^.   "ts" L._String
   let err = getResponseBody response ^? values.key  "error_msg" ._String
   case server of
    Just server -> TIO.putStrLn $  server
    Nothing -> putStrLn "nothing"
   case err of
        Just err -> TIO.putStrLn $  err
        Nothing -> putStrLn "nothingerr"
   putStrLn $ BS.unpack(getResponseBody response)

   case decodeStrict $ getResponseBody  response  :: Maybe Response of
    Just r -> print  r
    Nothing -> do
     case getErr $ getResponseBody response  of
      Nothing   -> putStrLn "Could not find the Bitcoin rate :("
      Just err -> TIO.putStrLn err
   putStrLn $ "The status code was: " ++
     show (getResponseStatusCode response)
   print $ getResponseHeader "Content-Type" response
   putStrLn $ BS.unpack(getResponseBody response)
   --BS.putStrLn   response

--}
