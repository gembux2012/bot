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
import qualified Data.Map as Map

import Network.Class
import Network.Types
import Network.Api
import Data.Text.Encoding (encodeUtf8)
import Data.Map (Map)
import Network.ErrorTypes 
import Network.Types (Message)


data LogCommand = MessageL Text | Stop (MVar ())

access =("","")
main :: IO ()
main = do
  conf <- readConfig
  m <- newEmptyMVar
  let config = snd conf
  putStrLn  $ fst conf
  let app = Application
            { logger = Logger {dologLn  = putMVar m . MessageL },                
              dorequest =DoRequest { doRequest = requestVK }
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
   => m (Either ErrorVK Message)
api  = botStart GetKeyAccess getKeyAccessUrl 
  
botStart m url = do
 Right (Access rsp)  <- request m url
 let access =rsp
 botStart SendMessage (getMessageUrl (BS8.pack(key access)) (BS8.pack(ts rsp)))
 {--
 case result of
  Left err -> logI.pack.show $ err 
  Right Access{..}  -> url = response'
  Right Message'{..} -> logI.pack.show $ show updates
 pure $ Right NoMessage
 --}
{--
botStart m url key ts = do
 --let access' = ("","")
 if m == GetKeyAccess then  
  logI "request an access key"
   else
    logI "waiting message"
 result <- request m url 
 case result of 
  Error err -> do 
   logE $ pack err
   pure NoResponse
  Auth (key,ts) -> do 
   --let access' =(key,ts)
   logI  " access key received"
   botStart GetMessage (getMessageUrl key ts ) key ts
  MessageVk MessageVK{..} -> do
   logI $ pack("message received: " ++ (show.text._object $ head updates) ++ "from: " 
      ++ (show.from_id._object $ head updates))    --show (from_id (_object (head updates))))
   case dispatcherAnswer MessageVK{..} of 
    SendMessage -> do 
      botStart SendMessage (sendMessageUrl  (BS8.pack.show.from_id._object $ head updates) 
       (BS8.pack.text._object $ head updates)) key (encodeUtf8 $ pack ts) 
      botStart GetMessage (getMessageUrl key (encodeUtf8 $ pack ts)) key (encodeUtf8 $ pack ts)
 
dispatcherAnswer Message{..} = 
 case text._object $ head updates  of -- /= "/" = botStart SendMessage   
   _ -> SendMessage 
 
 --}
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