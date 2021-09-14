{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Logger.App
  ( printLog,
  )
where

import Logger.Types (LogOpts (..))
import Control.Exception.Base (SomeException, handle, try)
import Control.Monad as CM
import Data.Text (Text, unpack)
import Data.Time as DT
import System.Directory (getFileSize, renamePath)
import System.FilePath.Posix (takeBaseName, takeExtension)



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
            (path ++ show (quantity - num))
        return ()
  where
    oldPath n q
      | n < q = path ++ show (q - n)
      | n == q = path
      | otherwise = path ++ "fghfg"

printLog :: LogOpts -> Text -> IO ()
printLog LogOpts {..} str =
  do
    td <-
      getZonedTime
        >>= \t ->
          return (formatTime defaultTimeLocale "%m-%d-%Y %H:%M:%S %Z" t)
    let logName = setLogName nameLog priority
    let strOut = td ++ " " ++ unpack str ++ "\n"
    sizeLogF <- fileSize $ pathToLog ++ logName
    _ <-
      rotationLog
        (pathToLog ++ logName)
        sizeLogF
        sizeLog
        maxNumFilesLog
   
    CM.when (displayMsg == 1) $
      do
        putStrLn strOut
        result <-
          try (appendFile (pathToLog ++ logName) strOut) ::
            IO (Either SomeException ())
        case result of
          Right _ -> return ()
          Left ex ->
            do
              putStrLn $
                show ex
                  <> ": cannot write a log, the app will be stopped , check the settings"
