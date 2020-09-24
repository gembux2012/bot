

--{-# LANGUAGE FlexibleContexts #-}

module Logger
( --logI,
  --logE,
  
 -- initLogger1,
     
    ) where
import System.IO
import Control.Exception
import Data.Time (getZonedTime, formatTime,defaultTimeLocale)
import Control.Lens (preview, (&))
import Data.Aeson.Lens (key, _String)
import Data.Aeson
--import Data.Text.Internal.Lazy (Text)
import qualified Data.ByteString.Char8         as BS
import           Data.Text                      ( Text, unpack, )
import qualified Data.Text.IO                  as TIO
import Data.Maybe 
import Config


data Priority =  INFO | NOTICE | WARNING | ERROR deriving (Show)

tD  =   getZonedTime >>= \t ->   
                return  (formatTime defaultTimeLocale "%m-%d-%Y %H:%M:%S %Z" t)

logY a = do
        t <- tD      
        putStrLn t
        
--logI msg  = logX msg  INFO

--initLogger1 (Config Logger {nameLogInfo = path})= path
 --  logX :: String-> Priority -> String ->  IO ()
   
  --logI msg  = logY  INFO

--logN fName = logX fName NOTICE
--logW fName = logX fName WARNING
--logE fName = logX fName ERROR


--getPath:: BS.ByteString -> Maybe Text
--getPath = preview (key "logger". key "pathToLog". _String)

--initLogger1 config  = print $ config 
--initLogger1 (Config (Logger path logSize logFilename printInConsole)) = logFilename = fName 

{--
initLogger  cfg  =  case  getPath cfg of
                              Nothing   -> putStrLn "Path not set"
                              Just  path -> print $ log   (unpack path) 
                              where  log path =Logger {pathToLog =   path, maxSizeLog = 1, nameLog = "", showToConsole = 1}
--}                            
 

--searchKeyFromJson:: BS.ByteString-> IO()
--searchKeyFromJson cfg   =print $ searchKey cfg   
          
--searchKeyFromJson keyString  cfg = return $ searchKey cfg     
--        where searchKey = preview(keyString)

