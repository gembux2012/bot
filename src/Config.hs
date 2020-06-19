{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Config 
  (
    
     Config(..),
     Logger(..),
     readConfig
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
import Control.Monad.Writer.Lazy (Writer)
import Control.Monad.RWS.Class (tell)



data Logger = Logger
        {
          pathToLog     :: String,
          maxSizeLog    :: Int,
          nameLog       :: String, 
          showToConsole :: Int
        }  deriving (Show)

data Config = Config {logger ::Logger} deriving(Show)
--data Config = Either String Config'

loggerDefault = Logger {pathToLog = "./out", maxSizeLog = 1, nameLog = "", showToConsole = 1}
settingsDefault = Config loggerDefault 

instance FromJSON Logger where
    parseJSON (Object logger) =
        Logger <$> logger .:? "pathToLog"     .!= pathToLog loggerDefault
               <*> logger .:? "maxSizeLog"    .!= maxSizeLog loggerDefault
               <*> logger .:? "nameLog"       .!= nameLog loggerDefault
               <*> logger .:? "showToConsole" .!= showToConsole loggerDefault
    parseJSON _ = mzero

instance FromJSON Config where
      parseJSON (Object o) = 
          Config <$> o .: "logger"  
      parseJSON _ = mzero     


warning = ", default values will be used!"
                    
readConfig :: IO (String,Config)
readConfig = do
             
             path <- getArgs
             case path of
             
                  []  ->  return $ ("Warning! no config set" ++ warning, settingsDefault)
                  [_] -> do
                         json <- try (B.readFile $ head path) :: IO (Either SomeException B.ByteString)
                         case json of
                           Left e -> return $ (show e ++ warning, settingsDefault)
                           Right context-> do
                             let result = decodeStrict context :: Maybe Config
                             case result of
                                 Nothing     -> return $ ("Warning! Invalid config fail" ++ warning, settingsDefault)
                                 Just cfg -> return $ ("", cfg)




 
                               
                                                
        
             
       
                      
                      
