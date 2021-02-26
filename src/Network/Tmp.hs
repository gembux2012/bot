module Network.Tmp where

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
module Main where

import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS8
import Network.HTTP.Simple (httpBS, setRequestHost, setRequestPath, setRequestQueryString, Query,
 defaultRequest, getResponseBody, getResponseStatusCode, httpJSON, setRequestIgnoreStatus,HttpException, setRequestMethod, getResponseHeader)
import qualified Data.ByteString as BS
import Control.Lens
import Control.Exception as E
import Network.HTTP.Client.Conduit (responseStatus, parseRequest)
import Network.HTTP.Types
import Control.Monad
import Control.Monad.Trans.Maybe (runMaybeT)
--import Network.HTTP.Client (HttpException)


--import Logging

idGroup = "202652768"

keyGroup = "13b47b20e6324c0dcc288baec4a318ee359bd16dc2f57c7bf215755241faf955de12b014a780ac7f9e455"

keyUser = "454751226"

versionService = 5.150

uriVK = "https://api.vk.com"

data RequestVK = RequestVK
  { requestHost :: BS8.ByteString,
    requestPath :: BS8.ByteString,
    requestMethod :: BS8.ByteString,
    requestQuery :: Query
  }

authRequestVk = RequestVK
 { requestHost = BS8.pack uriVK,
   requestPath = "method/",
   requestMethod = "groups.getLongPollServer",
   requestQuery = [("access_token", Just "57f5906e918c8dc83168e8d92770dde7610f53d0b143eb2030bc2f116532e07a463d0cdd82db544fc3efb"),
                    ("&group_id", Just "202652768"),("&v", Just "5.50")]
  }
  
main = requestVK authRequestVk
  
  
requestVK RequestVK{..} = do
  request' <- parseRequest  "https://api.vk.com" --method/groups.getLongPollServer?access_token=57f5906e918c8dc83168e8d92770dde7610f53d0b143eb2030bc2f116532e07a463d0cdd82db544fc3efb&group_id=202652768&v=5.50"
 
  let request 
        = setRequestMethod " GET "
        $ setRequestHost requestHost
        $ setRequestPath (requestPath <>  requestMethod)
        $ setRequestQueryString  requestQuery
        $ request'
        
 
  response <- httpBS request'
  -- print request'
  print $ getResponseStatusCode response
  
  let status = getResponseStatusCode response
  case status of
      200 -> do 
        --putStrLn $show  request
        --BS8.putStrLn $ getResponseBody response
        response ^? key "error" . key "error_msg" . _String >>= (Just >=> (putStrLn . show))  
      _ -> do
            putStrLn $   show requestHost  ++ " status " ++ show status
            
