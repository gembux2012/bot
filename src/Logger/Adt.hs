{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module Logger.Adt
  ( LogOpts (..),
    
    Priority (..),
    defaultLogOpts 
  )
where

import Data.Aeson.Types (FromJSON, withObject, parseJSON, (.:?), (.!=))
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Stack.Types (HasCallStack)

--newtype Logger m = Logger {dologLn :: HasCallStack => Text -> m ()}

data Priority = INFO | NOTICE | WARNING | ERROR deriving (Show, Eq, Ord)

data LogOpts = LogOpts
  { pathToLog :: String,
    nameLog :: String,
    sizeLog :: Integer,
    maxNumFilesLog :: Int,
    displayMsg :: Int,
    priority :: Int
  }
  deriving (Show)

instance FromJSON LogOpts where
   parseJSON = withObject "logops" $ \o -> do
     pathToLog <- o .:? "pathToLog" .!=  pathToLog defaultLogOpts
     nameLog <- o .:? "nameLog" .!= nameLog defaultLogOpts
     sizeLog <- o .:? "sizeLog" .!= sizeLog defaultLogOpts
     maxNumFilesLog <- o .:? "maxNumFilesLog" .!= maxNumFilesLog defaultLogOpts
     displayMsg  <- o .:? "displayMsg" .!= displayMsg defaultLogOpts
     priority  <- o .:? "priority" .!= priority defaultLogOpts
     return  LogOpts {..}


defaultLogOpts :: LogOpts
defaultLogOpts =
  LogOpts
    { pathToLog = "",
      nameLog = "bot.log",
      sizeLog = 250,
      maxNumFilesLog = 3,
      displayMsg = 1,
      priority = 1
    }
