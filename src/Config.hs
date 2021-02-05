{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Config 
  (

    Config(..),
    LogOpts(..),
    Subd(..),
    readConfig1,
     ) where 

import qualified Data.ByteString.Char8 as BS
import           Control.Monad (mzero)
import           Control.Monad.Reader
import           Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (fromMaybe)
import Data.Aeson.Text
import Data.Aeson.Lens (key, _String)
import System.Environment
import Control.Exception.Base (catch, throw, throwIO, try, SomeException)
import Control.Monad.Writer.Lazy (Writer)
import Control.Monad.RWS.Class (tell)
import Control.Lens.Combinators (preview)
import Data.Has (Has, getter)
import GHC.Generics (Generic)




data LogOpts = LogOpts
        {
          pathToLog   :: String,
          nameLogInfo :: String,
          displayMsg  :: Int
        }  deriving (Generic, ToJSON, FromJSON, Show)

data Subd =Subd
   {
     driver :: String,
     password :: String
   } deriving (Generic, ToJSON,FromJSON, Show)

data Config  = Config {logOpts ::LogOpts
                      , subd :: Subd
                       } deriving (Generic, ToJSON,FromJSON, Show)


warning = ", default values will be used!"





readConfig1 :: IO (Either String Config)

readConfig1 =  do
                 path <- getArgs
                 case path of
                   [] -> return $ Left    ("Warning! no config set" ++ warning)
                   [_] -> do
                     content <- try (BS.readFile $ head path) :: IO (Either SomeException BS.ByteString)
                     case content of
                       Left e -> return $ Left (show e ++ warning)
                       Right content ->  case decodeStrict content:: Maybe Config of
                                         Just config -> return $ Right config
                                         Nothing     -> return $ Left ("Invalid Json!! " ++ warning)












--}