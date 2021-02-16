{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}


module Network.Api where

import Data.Text.Internal.Lazy (Text)
import Data.Aeson.Types (ToJSON, FromJSON)
import GHC.Generics
import Network.HTTP.Simple (httpBS, getResponseBody)
import           Data.Text                      ( Text, append )

data Url = Url 
 {id         :: String,
  httpMethod :: String,
  methodService :: String, 
  parameters :: String,
  key :: String,
  versionService :: String 
 }
getLongPollServer = Url 
 {
 
 }

toUrl  httpM uri method parameters key v = uri ++ method  ++ parameters ++ key ++ v
run :: (Request -> IO Response) -> IO ()
run handler  d   
  = \url -> do 
  request <- parseRequest url
  response <- httpJSON request
  
  putStrLn $ "The status code was: " ++
            show (getResponseStatusCode response)
  print $ getResponseHeader "Content-Type" response
  S8.putStrLn $ d.encode (getResponseBody response :: d)
