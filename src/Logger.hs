{-# LANGUAGE OverloadedStrings #-}

module Logger
( logI,
  logE,
  logN,
  logW ,
  initConfig  
    ) where
import System.IO
import Control.Exception
import Data.Time (getZonedTime, formatTime,defaultTimeLocale)
import Control.Lens.Combinators (preview)
import Data.Aeson.Lens (key, _String)
import Data.Text.Internal.Lazy (Text)
import qualified Data.ByteString.Char8         as BS


data Logger = Logger
        {
          pathToLog     :: String,
          maxSizeLog    :: Int,
          nameLog       :: String, 
          showToConsole :: Int
        }  deriving (Show)
loggerDefault = Logger {pathToLog = "./out", maxSizeLog = 1, nameLog = "", showToConsole = 1}


getRate :: BS.ByteString -> Maybe Text
getRate = preview (key "bpi" . key "USD" . key "rate" . _String)

initConfig(Just cfg) =do
                       cfg
            --initConfig (Just cfg) = print   $  preview (key "logger" . key "pathToLog"._String) cfg
                                              
    --where setLog =preview (key "logger" . key "pathToLog"._String)   

data Priority =  INFO | NOTICE | WARNING | ERROR deriving (Show)



logX :: String -> Priority -> String ->  IO ()
logX fName priority msg =   getZonedTime >>= \t ->   
              appendFile fName (formatTime defaultTimeLocale "%m-%d-%Y %H:%M:%S %Z" t ++ " " ++ show priority ++ " " ++ msg ++ "\n")

logI fName = logX fName INFO
logN fName = logX fName NOTICE
logW fName = logX fName WARNING
logE fName = logX fName ERROR