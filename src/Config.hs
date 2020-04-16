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
import Control.Exception.Base (catch, throw, throwIO, try, SomeException)
import Logger
import Control.Monad.Writer.Lazy (Writer)
import Control.Monad.RWS.Class (tell)



data Logger = Logger
        {
          pathToLog     :: String,
          maxSizeLog    :: Int,
          showToConsole :: Int
        }  deriving (Show)

newtype Config = Config{logger :: [Logger]} deriving (Show)
--data Config = Either String Config'

options = Logger {pathToLog = "./out", maxSizeLog = 1 , showToConsole = 1}


instance FromJSON Logger where
    parseJSON (Object v) =
        Logger <$> v .:? "pathToLog"     .!= pathToLog options
               <*> v .:? "maxSizeLog"    .!= maxSizeLog options
               <*> v .:? "showToConsole" .!= showToConsole options
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





 
                               
                                                
        
             
       
                      
                      
