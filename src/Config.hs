{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Config 
  (
     settings,
     Config(..),
     Logger(..)
     ) where 

import qualified Data.ByteString as B
import           Control.Monad (mzero)
import           Control.Monad.Reader
import           Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromMaybe)
import Data.Aeson.Text
import System.Environment
import Control.Exception.Base (catch, throw, throwIO, try, SomeException)
import Logger
import Control.Monad.Writer.Lazy (Writer)
import Control.Monad.RWS.Class (tell)



data Logger = Logger
        {
          pathToLog     :: String,
          maxSizeLog    :: Int,
          nameLog       :: String, 
          showToConsole :: Int
        }  deriving (Show)

newtype Config = Config{logger :: Logger} deriving (Show)
--data Config = Either String Config'

loggerDefault = Logger {pathToLog = "./out", maxSizeLog = 1, nameLog = "", showToConsole = 1}
settingsDefault = Config loggerDefault

instance FromJSON Logger where
    parseJSON (Object v) =
        Logger <$> v .:? "pathToLog"     .!= pathToLog loggerDefault
               <*> v .:? "maxSizeLog"    .!= maxSizeLog loggerDefault
               <*> v .:? "nameLog"       .!= nameLog loggerDefault
               <*> v .:? "showToConsole" .!= showToConsole loggerDefault
    parseJSON _ = mzero

instance FromJSON Config where
      parseJSON (Object o) = 
          Config <$> o .: "logger"  
      parseJSON _ = mzero     


warning = ", default values will be used!"

readConfig :: IO (Either String Config)
readConfig = do
             path <- getArgs
             case path of
                  []  -> return $ Left ("no config set" ++ warning)
                  [_] -> do
                         json <- try (B.readFile $ head path) :: IO (Either SomeException B.ByteString)
                         case json of
                           Left e -> return $ Left(show e ++ warning)
                           Right context-> do
                             let result = decodeStrict context :: Maybe Config
                             case result of
                                 Nothing     -> return $ Left ("Invalid config fail" ++ warning)
                                 Just config -> return $ Right config

settings:: IO (String, Config)
settings =do 
          settings <- readConfig
          case settings of
           Left val -> return (val, settingsDefault)
           Right cfg -> return ("" , cfg)



 
                               
                                                
        
             
       
                      
                      
