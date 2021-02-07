{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Log
  ( 
    Log (..),
    Logger (..),
    printLog
  )
where

import Config (LogOpts (..))
import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.Reader.Class (asks)
import Data.Has (Has (..))
import Data.Text (Text, append, pack, unpack, isInfixOf,)
import Data.Time as DT
import GHC.Stack (HasCallStack)
import Prelude
import Control.Exception.Base (SomeException, try, bracket, handle)
import GHC.IO.IOMode (IOMode(..))
import GHC.IO.Handle (hClose, hFileSize, Handle)
import Control.Monad as CM
import System.IO (openFile)
import System.FilePath.Posix(takeBaseName, takeExtension)
import Control.Monad.Trans.Maybe

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
                  $ bracket (openFile path ReadMode) hClose (\h -> do size <- hFileSize h
                                                                      return $ Just size)
  where
    handler :: SomeException -> IO (Maybe Integer)
    handler _ = return Nothing

getFileSize' :: FilePath -> (Handle -> MaybeT IO Integer) ->IO() 
getFileSize' path =   bracket (openFile path ReadMode) hClose (\h -> do  lift  hFileSize h)
  where
   maybeSize h  = do lift $ hFileSize h
    

    
setLogName :: String -> Int -> String -> Integer -> String
setLogName name pr path sizelog    
      | pr == 1 = takeBaseName name ++ ".error" ++ takeExtension name ++ rotationLog name
      | otherwise = name
      where 
         rotationLog name 
           | (getFileSize path)(Just size) >= sizelog ="1" 
              
              

printLog :: LogOpts -> Text -> IO ()    -- Log                 
printLog LogOpts{..} str
  = do td <- getZonedTime >>= \t -> return  (formatTime defaultTimeLocale "%m-%d-%Y %H:%M:%S %Z" t)
       let logName = setLogName nameLog priority pathToLog sizeLog  
       let strOut = td ++ " " ++ unpack str ++ "\n" 
       CM.when (displayMsg == 1) $ do putStrLn strOut
       result <- try (appendFile (pathToLog ++ logName) strOut) ::
                   IO (Either SomeException ())
       case result of
         Right  _ -> return ()
         Left ex
           -> do putStrLn
                   $ show ex
                       <>
                         ": cannot write a log, the app will be stopped , check the settings"
                         
          