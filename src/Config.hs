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
    LogOpts (..),
    Subd (..),
    readConfig1,
  )
where

import           Control.Exception.Base    (SomeException, catch, throw,
                                            throwIO, try)
import           Control.Lens.Combinators  (preview)
import           Control.Monad             (mzero)
import           Control.Monad.Reader
import           Control.Monad.RWS.Class   (tell)
import           Control.Monad.Writer.Lazy (Writer)
import           Data.Aeson
import           Data.Aeson.Lens           (key, _String)
import           Data.Aeson.Text
import           Data.Aeson.Types          (parseMaybe)
import qualified Data.ByteString.Char8     as BS
import           Data.Has                  (Has, getter)
import           Data.Maybe                (fromMaybe)
import           GHC.Generics              (Generic)
import           System.Environment

data LogOpts = LogOpts
  { pathToLog      :: String,
    nameLog        :: String,
    sizeLog        :: Integer,
    maxNumFilesLog :: Int,
    displayMsg     :: Int,
    priority       :: Int
  }
  deriving (Generic, ToJSON, FromJSON, Show)

data Subd = Subd
  { driver   :: String,
    password :: String
  }
  deriving (Generic, ToJSON, FromJSON, Show)

data Config = Config
  { logOpts :: LogOpts,
    subd    :: Subd
  }
  deriving (Generic, ToJSON, FromJSON, Show)

warning = ", default values will be used!"

readConfig1 :: IO (Either String Config)
readConfig1 = do
  path <- getArgs
  case path of
    [] -> return $ Left ("Warning! no config set" ++ warning)
    [_] -> do
      content <- try (BS.readFile $ head path) :: IO (Either SomeException BS.ByteString)

      case content of
        Left e -> return $ Left (show e ++ warning)
        Right content -> case decodeStrict content :: Maybe Config of
          Just config -> return $ Right config
          Nothing     -> return $ Left ("Invalid Json!! " ++ warning)


