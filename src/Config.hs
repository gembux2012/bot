{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Config 
  (
     readConfig,
     ) where 

import qualified Data.ByteString as B
import           Control.Monad (mzero)
import           Control.Monad.Reader
import           Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromMaybe)
import Data.Aeson.Text
import System.Environment
import Control.Exception.Base (catch, throw, throwIO)
import Logger
import Exception (ConfigError(..))



data Logger = Logger
        {
          pathToLog     :: String,
          maxSizeLog    :: Int,
          showToConsole :: Int
        }  deriving (Show)

data Config' = Config' { logger :: [Logger]} deriving (Show)
data Config = Either String Config'

options = Logger {pathToLog = "./out", maxSizeLog = 1 , showToConsole = 1}


instance FromJSON Logger where
    parseJSON (Object v) =
        Logger <$> v .:? "pathToLog"     .!= pathToLog options
               <*> v .:? "maxSizeLog"    .!= maxSizeLog options
               <*> v .:? "showToConsole" .!= showToConsole options
    parseJSON _ = mzero

instance FromJSON Config' where
      parseJSON (Object o) = 
          Config' <$> o .: "logger"  
      parseJSON _ = mzero     



readConfig :: IO Config

readConfig = do
               path <- getArgs
               return( case path of
                      [] -> Left  "uiiouoiu"
                      [_] -> Right head Path do 
                                    json <- B.readFile path 
                                    return())
                                      
                      

 {--              
getPath = do
  
readConfig = do
             return(        
              case getPath of
                Left  msg -> Left msg
                Right path -> Right do 
                            rawJSON <- B.readFile path 
                            let result = decodeStrict rawJSON :: Maybe [Config]
                            putStrLn $ case result of
                                   Nothing    -> "Invalid JSON!"
                                   Just config -> show  config )

                               
                                                
        
             
       
                      
                      
                      {-
                      let result = decodeStrict drawJSON :: Maybe Config 
                      o <-  case result of
                            Nothing   ->  "Invalid JSON!"
                            Just content -> show content
                        -}    
                        --}