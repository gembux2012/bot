{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UndecidableInstances  #-}

module Config
  ( Config (..),
    readConfig,
  )
where

import           Control.Exception.Base            (try)
import           Data.Aeson
import qualified Data.ByteString.Char8             as BS
import           GHC.Generics                      (Generic)
import           System.Directory.Internal.Prelude (getArgs)

import           Logger.Adt                        (LogOpts, defaultLogOpts)


newtype Config
  = Config {logOpts :: LogOpts}
  deriving (Generic,  FromJSON, Show)

defaultConfig= Config
  { logOpts = defaultLogOpts
  
  }
warning :: [Char]
warning = ", default values will be used!"

readConfig :: IO (String, Config)
readConfig = do
  path <- getArgs
  case path of
    [] -> return   ("Warning! no config set" ++ warning, defaultConfig)
    [_] -> do
      content <- try (BS.readFile $ head path) :: IO (Either IOError BS.ByteString)
      case content of
        Left e -> return (show e ++ warning, defaultConfig)
        Right content' -> case decodeStrict content' :: Maybe Config of
          Just config -> return ("config read ", config)
          Nothing     ->  return ("Invalid Json!! " ++ warning, defaultConfig)


