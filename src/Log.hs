{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

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
import Control.Exception.Base (SomeException, try)
import Data.String



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



setFname name  str     
      | "info" `isInfixOf` pack name  && ("ERROR" `isInfixOf` str) = "bot.error.log"
      | otherwise = name
      
tD  =   getZonedTime >>= \t ->   
                return  (formatTime defaultTimeLocale "%m-%d-%Y %H:%M:%S %Z" t)
                 
printLog  path fName str 
  = do let fName' = setFname fName str  
       t <- tD
       let strOut = t ++ " " ++ unpack str ++ "\n"
       result <- try (appendFile (path ++ fName') strOut) ::
                   IO (Either SomeException ())
       case result of
         Right val -> return ()
         Left ex
           -> do putStrLn
                   $ show ex
                       <>
                         ": cannot write a log, the app will be stopped , check the settings"
      