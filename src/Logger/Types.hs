{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module Logger.Types

where

import Data.Aeson.Types (FromJSON, withObject, parseJSON, (.:?), (.!=))
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Stack.Types (HasCallStack)

data Priority = INFO | NOTICE | WARNING | ERROR deriving (Show, Eq, Ord)

data Config  = Config 
  {logOpts :: LogOpts,
   vkOpts :: VKOpts
  }
  deriving (Generic,  FromJSON, Show)

defaultConfig= Config
  { logOpts = defaultLogOpts
  
  }

newtype VKOpts = VKOpts
 { button :: String
 }
 deriving (Generic,  FromJSON, Show)
 
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
