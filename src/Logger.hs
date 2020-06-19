module Logger 
( logI,
  logE,
  logN,
  logW ,
  --loggerInit  
    ) where
import System.IO
import Control.Exception
import Data.Time (getZonedTime, formatTime,defaultTimeLocale)
import Config 

--loggerInit config = putStrLn $  Logger config
--fName = pathToLog Logger Config

data Priority =  INFO | NOTICE | WARNING | ERROR deriving (Show)



logX :: String -> Priority -> String ->  IO ()
logX fName priority msg =
  getZonedTime >>= \t ->
    appendFile fName (formatTime defaultTimeLocale "%m-%d-%Y %H:%M:%S %Z" t ++ " " ++ show priority ++ " " ++ msg ++ "\n")

logI fName = logX fName INFO
logN fName = logX fName NOTICE
logW fName = logX fName WARNING
logE fName = logX fName ERROR