{-# LANGUAGE OverloadedStrings #-}
module Config 
  (
     readJson
     ) where 

import qualified Data.ByteString as B
import           Control.Monad (mzero)
import           Control.Monad.Reader
import           Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromMaybe)
import Data.Aeson.Text
import System.Environment
import Exception (fileException)
import Control.Exception.Base (catch)


data Logger = Logger
        {
          pathToLog     :: String,
          maxSizeLog    :: Int,
          showToConsole :: Int
        }  deriving (Show)

data Config = Config { logger :: [Logger]} deriving (Show)


instance FromJSON Logger where
    parseJSON (Object v) =
        Logger <$> v .: "pathToLog"
               <*> v .: "maxSizeLog"
               <*> v .: "showToConsole"
    parseJSON _ = mzero

instance FromJSON Config where
      parseJSON (Object o) = 
          Config <$> o .: "logger"    

readJson =do
      path <-  getArgs 
      case path of
            [] -> putStrLn  " Warning ! the path to the config is not specified, the default settings will be used"
            [path ,_] -> do drawJSON <- B.readFile  path `catch` fileException
                            let result = decodeStrict drawJSON :: Maybe Config
                            putStrLn $ case result of
                                Nothing   ->  "Invalid JSON!"
                                Just content ->  show content 


