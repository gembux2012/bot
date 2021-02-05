{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Log
  ( 
    Log (..),
    Logger (..),
    Priority (..),
    SetOpts,
    printLog
  )
where

import Config (Config, LogOpts (..), logOpts, )
import Control.Monad.Reader (ReaderT, lift, void)
import Control.Monad.Reader.Class (asks)
import Data.Has (Has (..))
import Data.Text (Text, append, pack, unpack, isInfixOf,)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime, ZonedTime, getZonedTime)
import GHC.Stack (HasCallStack)
import Prelude
import Control.Exception.Base (SomeException, try, handle, bracket)
import Data.String
import GHC.IO.FD (openFile)
import GHC.IO.IOMode (IOMode(..))
import GHC.IO.Handle (hClose, hFileSize)
import Control.Monad as CM


class Monad m => Log m where
  logI  :: HasCallStack => Text -> m ()
  logW  :: HasCallStack => Text -> m ()
  logE  :: HasCallStack => Text -> m ()

newtype Logger m = Logger {dologLn :: HasCallStack => Text -> m ()}




data Priority = INFO | NOTICE | WARNING | ERROR deriving (Show, Eq, Ord)

newtype SetOpts m = SetOpts {logOpt :: LogOpts -> Text -> m ()}


{--
defaultLogOpts =
  LogOpts
    { pathToLog = "kjk",
      nameLogInfo = "bot.log"

    }
--}





instance
  ( Has (Logger m) r,
    Monad m
  ) =>
  Log (ReaderT r m)
  where
   
   logI  t =    
      asks getter >>= \(Logger  doLog) -> lift . doLog $  (pack . show $ INFO) `append` " " `append`  t
   logW  t =    
      asks getter >>= \(Logger  doLog) -> lift . doLog $ (pack . show $ WARNING) `append` " " `append`  t
   logE  t =    
      asks getter >>= \(Logger  doLog) -> lift . doLog $ (pack . show $ ERROR) `append` " " `append`  t

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle handler
                  $ bracket (openFile path ReadMode) (hClose ) (\h -> do size <- hFileSize h
                                                                         return $ Just size)
  where
    handler :: SomeException -> IO (Maybe Integer)
    handler _ = return Nothing

setLogName name  str     
      | "info" `isInfixOf` pack name  && ("ERROR" `isInfixOf` str) = "bot.error.log"
      | otherwise = name
      

printLog :: LogOpts -> Text -> IO ()    -- Log                 
printLog LogOpts{..} str
  = do td <- getZonedTime >>= \t -> return  (formatTime defaultTimeLocale "%m-%d-%Y %H:%M:%S %Z" t)
       let logName = setLogName   nameLogInfo  str 
       let strOut = td ++ " " ++ unpack str ++ "\n" 
       CM.when (displayMsg == 1) $ do putStrLn strOut
       result <- try (appendFile (pathToLog ++ logName) strOut) ::
                   IO (Either SomeException ())
       case result of
         Right val -> return ()
         Left ex
           -> do putStrLn
                   $ show ex
                       <>
                         ": cannot write a log, the app will be stopped , check the settings"
                         
          