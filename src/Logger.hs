
{-# LANGUAGE RecordWildCards #-}
--{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE BlockArguments #-}

module Logger
( --logI,
  --logE,
  
 -- initLogger1,
     
    ) where
--import System.IO
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

newtype Handle m
  = Handle
      { writeLog :: String -> m ()
      }

logX :: LogOpts ->String -> Handle IO
logX  options str = Handle {writeLog = \log  -> print log}

logInit  opt   = do
   let str = "ok"
   let handle  = Handle {writeLog = writeLog (logX opt str)}
   logOut handle "ok"

logOut :: Monad m => Handle m -> String -> m ()
logOut Handle {..} str = writeLog str

logY str = logOut Handle {..} str
  