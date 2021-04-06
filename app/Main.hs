{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import           Config                 (Config (..), readConfig)
import           Control.Exception.Base ()
import           Control.Monad.Reader   (runReaderT, MonadIO)
import           Data.Has               (Has, getter, modifier)
import           GHC.Generics           (Generic)
import           Logger.App             (printLog)

import           Logger.Adt             (Logger(..), )
import           Logger.Class           (Log (..))
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text, pack, unpack)
--import Data.ByteString.Char8 (pack)
import Network.Api 
import Control.Monad.Catch.Pure (MonadThrow)
import Control.Monad.Base (MonadBase)
import Control.Concurrent

import Network.Class
import Network.Types
import Network.Api


data LogCommand = MessageL Text | Stop (MVar ())


main :: IO ()
main = do
  conf <- readConfig
  m <- newEmptyMVar
  let config = snd conf
  putStrLn  $ fst conf
  let app = Application
            { logger = Logger {dologLn  = putMVar m . MessageL },                
              dorequest = DoRequest{ doRequest = requestVK } 
            }
  forkIO $ logger' config m
  runReaderT api  app
 
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s             

 
   
logger' :: Config -> MVar LogCommand -> IO ()
logger' conf m  = loop 
 where  
   loop   = do
    cmd <- takeMVar m
    case cmd of
     MessageL msg -> do
       printLog  (logOpts conf)  msg 
       loop      
     Stop s -> do
          putMVar s ()

api :: 
   Log m 
   => GetMessageVK m
   => m ResponseMessage
api = botStart GetKeyAccess getKeyAccessUrl  

botStart m url = do
 logI "bot start"
 logI "request an access key"
 result <- request m url 
 case result of 
  Error err -> do 
   logE "err"
   pure NoResponse
  Auth (key,ts) -> do 
   logI  " access key received"
   botStart GetMessage (getMessageUrl key ts)
  MessageVk mess -> do
   logI $ pack(show mess)
   pure ()   
receivingMessage key ts = do
 botStart GetMessage (getMessageUrl key ts)
 receivingMessage key ts
 
data Access  = Access
 { key :: BS8.ByteString,
   ts :: BS8.ByteString
 } 
 
access k t =  Access 
 { key = k,
   ts = t
 }
 
data  Application m   = Application 
  {logger :: Logger m ,
   dorequest :: DoRequest m 
  }
  deriving stock Generic

instance Has (Logger m) (Application m) where
  getter = logger
  modifier f a = a {logger = f . logger $ a}

instance Has (DoRequest m) (Application m) where
  getter = dorequest
  modifier f a = a {dorequest = f . dorequest $ a}