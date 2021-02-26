{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import           Control.Lens ((^.), (^?))
import           Control.Lens.Combinators (preview)
import qualified Control.Monad as CM
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Aeson.Lens as L
import           Data.Aeson.Lens (_String, key)
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

versionService = 5.150

uriVK = "https://api.vk.com/"

-- {"response":{"key":"e2d4c232a80a977a0211d2f0093767032bbd5635","server":"https:\/\/lp.vk.com\/wh202652768","ts":10}}
data RequestVK = RequestVK
  { requestHost :: BS8.String,
    requestPath :: BS8.String,
    requestMethod ::BS8.String,
    requestQuery :: Query
  }

authRequestVk = RequestVK
 { requestHost = uriVK,
   requestPath = "/method/",
   requestMethod = "groups.getLongPollServer?",
   requestQuery = [("access_token", Just keyGroup),("group_id", Just idGroup),("v", Just versionService) ]
  }

getMessageRequestVK = RequestVK
 { requestHost = uriVK,
    requestPath = "",
    requestMethod = "",
    requestQuery = [("access_token", Just keyGroup),("group_id", Just idGroup),("v", Just versionService) ]
   }

requestVK ::
  MonadThrow m =>
  MonadIO m =>
  MonadBase IO m =>
  Log m =>
  RequestVK ->
  m BS8.ByteString
requestVK req = do
  let request 
        = setRequestHost uriVK
        $ setRequestPath requestPath <> requestMethod
        $ setRequestQueryString requestQuery
        -- $ defaultRequest
   --rq <- parseRequest request
  response <- httpBS request
  let status = getResponseStatusCode response
  case status of
    200 -> do
      case method of
        "outh" -> do
          case getResponseBody response ^? key "error" . key "error_msg" . _String of
            Just err -> do
              logE $ T.pack (show rq) <> " return " <> T.pack (show err) <> " app will be stopped"
              return "/stop"
            Nothing -> do
              case extractMessage (getResponseBody response) "outh" of
                Just urlM -> do
                  logI $ T.pack (show rq) <> " status " <> T.pack (show status)
                  requestVK urlM "getMessage"
                Nothing -> return "/stop"
        "getMessge" -> do
          --case getResponseBody response ^? key "failed" . _String of
            err <- getResponseBody response ^? key "failed" . _String
            ts <- err ^? key "failed" . key "ts" _String
            case err of
             1 -> do

            Nothing -> do
              case extractMessage (getResponseBody response) "outh" of
                Just urlM -> do
                  logI $ T.pack (show rq) <> " status " <> T.pack (show status)
                  requestVK urlM "getMessage"
                Nothing -> return "/stop"
    _ -> do
      logW $ T.pack (show rq) <> " status " <> T.pack (show status)
      CCL.threadDelay $ 10 ^ 7
      requestVK url method

extractMessage body method = do
  case method of
    "outh " -> do
      keyAuth <- body ^? key "response" . key "key" . _String
      server <- body ^? key "response" . key "server" . _String
      ts <- body ^? key "response" . key "ts" . _String
      let url =
            uri ++ "/" ++ T.unpack server ++ "?act=a_check&key=" ++ T.unpack keyAuth
              ++ "&ts="
              ++ T.unpack ts
              ++ "&wait=25"
      return url



run ::
  MonadThrow m =>
  MonadIO m =>
  MonadBase IO m =>
  Log m =>
  RequestVK ->  m ()
run authRequestVk =
  do
    body <- requestVK url
    logI $ T.pack (BS8.unpack body)

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
