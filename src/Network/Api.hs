{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE Strict #-}



module Network.Api
 ( run

 )
where

-- import Data.Text.Internal.Lazy (Text)
import Data.Aeson -- .Types (ToJSON, FromJSON)
import qualified Data.Aeson.Lens  as L
import GHC.Generics
import Network.HTTP.Simple (httpBS, getResponseBody, parseRequest, httpJSON, getResponseStatusCode, getResponseHeader, httpJSONEither, HttpException, JSONException)
import           Data.Text                      ( Text, append )
import qualified Data.Text.IO                  as TIO
import qualified Data.ByteString.Char8 as BS
-- import Data.Aeson.Encode (encode)
import Network.HTTP.Conduit (http)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Control.Lens.Combinators (preview)

import           Logger.Class           (Log (..))
import Data.ByteString.Char8 (pack)

--https://api.vk.com/method/groups.getLongPollServer?access_token=&group_id=202652768&v=5.130
-- send https://api.vk.com/method/messages.send?user_id=454751226&message=&title=gh&access_token=v=5.50
-- outh https://api.vk.com/method/groups.getLongPollServer?access_token=57f5906e918c8dc83168e8d92770dde7610f53d0b143eb2030bc2f116532e07a463d0cdd82db544fc3efb&group_id=202652768&v=5.50
idCommunity = "202652768"
keyCommunity  ="13b47b20e6324c0dcc288baec4a318ee359bd16dc2f57c7bf215755241faf955de12b014a780ac7f9e455"
keyUser = "454751226"
versionService  = "5.150"
uri = "https://api.vk.com/method/"


-- {"response":{"key":"e2d4c232a80a977a0211d2f0093767032bbd5635","server":"https:\/\/lp.vk.com\/wh202652768","ts":10}}
data ResponseAuth = 
   ResponseAuth{
    --response :: String,
    key :: String,
    server :: String,
    ts :: String
   }
   deriving (Generic,  FromJSON, Show)

newtype Response
  = Response {response :: ResponseAuth}
  deriving (Generic, FromJSON, Show)
 

auth =  uri ++ "groups.getLongPollServer?access_token=" ++ keyCommunity ++
         "&group_id=" ++ idCommunity ++"&v=" ++ versionService

getErr :: BS.ByteString -> Maybe Text

getErr = preview (L.key "error" . L.key "error_msg" .  L._String)





request
  =  do
   request <- parseRequest $ "GET" ++ " " ++ auth
   putStrLn auth 
   response <- httpBS  request
    
   case decodeStrict $ getResponseBody  response  :: Maybe Response of
    Just r -> print  r
    Nothing -> do
     case getErr $ getResponseBody response  of
      Nothing   -> putStrLn "Could not find the Bitcoin rate :("
      Just err -> TIO.putStrLn err 
   putStrLn $ "The status code was: " ++
     show (getResponseStatusCode response)
   print $ getResponseHeader "Content-Type" response
   --putStrLn $ BS.unpack(getResponseBody response)
   --BS.putStrLn   response
 
