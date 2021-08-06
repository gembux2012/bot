{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Config (Config (..), readConfig)
--import Data.ByteString.Char8 (pack)

import Control.Applicative
import Control.Concurrent
import Control.Exception.Base ()
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch.Pure (MonadThrow)
import Control.Monad.Reader
  ( MonadIO,
    Reader,
    ask,
    liftM,
    runReaderT,
    when,
  )
import qualified Data.ByteString.Char8 as BS8
import Data.Has (Has, getter, modifier)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import GHC.Generics (Generic)
import Logger.Adt
import Logger.App (printLog)
import Logger.Class
import Network.Api
import Network.Api
import Network.Class
import Network.ErrorTypes
import Network.Types
import Network.Types (Message)
import Network.Types (Message)
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (encode)
--import Data.Aeson.Encode (encode)
--import Data.Aeson (encode)

data LogCommand = MessageL Text | Stop (MVar ())

main :: IO ()
main = do
  conf <- readConfig
  m <- newEmptyMVar
  let config = snd conf
  putStrLn $ fst conf

  let app =
        Application
          { logger = Logger {dologLn = putMVar m . MessageL}
          --dorequest = DoRequest { doRequest = requestVK }
          }

  forkIO $ logger' config m

  runReaderT (api getKeyAccessUrl) app

  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s

logger' :: Config -> MVar LogCommand -> IO ()
logger' conf m = loop
  where
    loop = do
      cmd <- takeMVar m
      case cmd of
        MessageL msg -> do
          printLog (logOpts conf) msg
          loop
        Stop s -> do
          putMVar s ()

api ::
  Log m =>
  MonadIO m =>
  MonadThrow m =>
  MonadFail m =>
  Url ->
  m Message
api url = do
  logI  " get access "
  resp <- requestVK url
  case resp of
    Access access -> do
      logI  " accessed, awaiting messages "
      logI $ key access
      getMessage  (BS8.pack $ key access)
                  (BS8.pack $ server access) 
                  (BS8.pack $ ts access)
    _ -> do
      logI $ pack . show $ resp
      pure NoMessage

getMessage :: 
   Log m =>
   MonadIO m =>
   MonadThrow m =>
   MonadFail m => 
   BS8.ByteString -> BS8.ByteString -> BS8.ByteString -> m Message 
getMessage key server ts = do
  resp <- requestVK $ getMessageUrl key server ts
  case resp of
    Message' ts' updates -> 
      if not (null updates)
        then do
          mapM_
            ( \MessageUpdates {..} -> 
                case  _type of
                  "message_new"  -> do
                      logI $ pack "event: "  <>  pack _type
                      logI $ pack (show resp)
                      logI $ pack  "received message from user id :"  <> pack (show from_id) <> pack " " <> pack text
                      sendMessage (BS8.pack . show $ from_id) text _payload 
                  _ -> logI $ pack "event: "  <>  pack _type
            )
            updates
          logI  " awaiting messages "
          getMessage key server (BS8.pack ts')
        else do
          logI  " awaiting messages "
          getMessage key server ts
    _ -> do
        logI $ pack . show $ resp  
        pure NoMessage    
    
sendMessage :: 
    Log m =>
    MonadIO m =>
    MonadThrow m =>
    MonadFail m => 
 BS8.ByteString -> String -> String -> m () 
sendMessage id text btn = do
  logI $ pack $ "send message to user id:" <> BS8.unpack id <> " " <> text 
  --logI $ BS8.unpack.LBS.toStrict $ encode repeatButtons
  let answer = dispatcherAnswer text btn
  resp <- requestVK $ uncurry (sendMessageUrl id) answer 
  case resp of
    Response rsp -> do
      logI $ pack ("recponse code " <> show rsp)
    _ -> do
          logI $ pack . show $ resp
          --pure NoMessage  

dispatcherAnswer :: String -> String ->  ( BS8.ByteString,  BS8.ByteString)
dispatcherAnswer ['\\','r','e','p','e','a','t'] _ = (BS8.pack "how many times repeat message ?",  
                                                   LBS.toStrict $ encode repeatButtons)
dispatcherAnswer str btn = (BS8.pack (concat [str <> " "| r <- [0.. read btn]]), 
                            LBS.toStrict $ encode emptyButtons)

 --}
data Application m = Application
  { logger :: Logger m
  --dorequest :: DoRequest m
  }
  deriving stock (Generic)

instance Has (Logger m) (Application m) where
  getter = logger
  modifier f a = a {logger = f . logger $ a}
