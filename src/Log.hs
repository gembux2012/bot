{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Log
  ( Log (..),
    Logger (..),
    printLog,
  )
where

import Config (LogOpts (..))
import Control.Exception.Base (SomeException, handle, try)
import Control.Monad as CM
import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans.Maybe ()
import Data.Char ()
import Data.Has (Has (..))
import Data.Text (Text, append, pack, unpack)
import Data.Time as DT
import GHC.IO.Handle ()
import GHC.IO.IOMode ()
import GHC.Stack (HasCallStack)
import System.Directory (getFileSize, renamePath)
import System.FilePath.Posix (takeBaseName, takeExtension)
import System.IO ()
import Prelude

class Monad m => Log m where
  logI :: HasCallStack => Text -> m ()
  logW :: HasCallStack => Text -> m ()
  logE :: HasCallStack => Text -> m ()

newtype Logger m = Logger {dologLn :: HasCallStack => Text -> m ()}

data Priority = INFO | NOTICE | WARNING | ERROR deriving (Show, Eq, Ord)



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
  logI t =
    asks getter >>= \(Logger doLog) -> lift . doLog $ (pack . show $ INFO) `append` " " `append` t
  logW t =
    asks getter >>= \(Logger doLog) -> lift . doLog $ (pack . show $ WARNING) `append` " " `append` t
  logE t =
    asks getter >>= \(Logger doLog) -> lift . doLog $ (pack . show $ ERROR) `append` " " `append` t

fileSize :: FilePath -> IO Integer
fileSize path = handle handler (do getFileSize path)
  where
    handler :: IOError -> IO Integer
    handler _ = return 0

setLogName :: String -> Int -> String
setLogName name pr
  | pr == 1 = takeBaseName name ++ ".error" ++ takeExtension name
  | otherwise = name

renMovF :: String -> String -> IO ()
renMovF oldPath newPath = handle handler (do renamePath oldPath newPath)
  where
    handler :: IOError -> IO ()
    handler _ = return ()

rotationLog :: String -> Integer -> Integer -> Int -> IO ()
rotationLog path sizelogf sizelog quantity =
  CM.when (sizelogf >= sizelog) $ do forM_
    [1 .. quantity - 1]
    \num ->
      do
        _ <-
          renMovF
            (oldPath num (quantity - 1))
            (path ++ show (quantity - num ))
        return ()
  where
    oldPath n q
      | n < q = path ++ show (q - n)
      | n == q = path
      | otherwise = path ++"fghfg"

printLog :: LogOpts -> Text -> IO () 
printLog LogOpts {..} str =
  do td <- getZonedTime
             >>=
               \ t
                 -> return (formatTime defaultTimeLocale "%m-%d-%Y %H:%M:%S %Z" t)
     let logName = setLogName nameLog priority
     let strOut = td ++ " " ++ unpack str ++ "\n"
     sizeLogF <- fileSize $ pathToLog ++ logName
     _ <- rotationLog
            (pathToLog ++ logName) sizeLogF sizeLog maxNumFilesLog
     print sizeLogF
     print sizeLog
     CM.when (displayMsg == 1)
       $ do putStrLn strOut
            result <- try (appendFile (pathToLog ++ logName) strOut) ::
                        IO (Either SomeException ())
            case result of
              Right _ -> return ()
              Left ex
                -> do putStrLn
                        $ show ex
                            <>
                              ": cannot write a log, the app will be stopped , check the settings"
   
