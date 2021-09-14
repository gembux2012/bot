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
import Control.Monad.RWS.Lazy (RWST, evalRWST, execRWST, get, runRWST, tell)
import Control.Monad.Reader
  ( MonadIO,
    Reader,
    ask,
    liftM,
    runReaderT,
    when,
  )
import Control.Monad.State.Class
import Control.Monad.State.Lazy (execStateT, runStateT)
import Control.Monad.Trans.Writer.CPS (Writer, WriterT)
import Control.Monad.Writer
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.UTF8
import Data.Has (Has, getter, modifier)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Logger.Types
import Logger.App (printLog)
import Logger.Class
import Network.App
import Network.Class
import Network.Types
import Streams
import MutableList



type ListUser   = Map String Int
newtype ListUserState = ListUserState (MVar ListUser)

main :: IO ()
main = do
  conf <- readConfig
  let config = snd conf
  putStrLn $ fst conf
  l <- initLogger config
  listUser <- newList
  let app  =
        Application
          { requests =
            Requests
            { doGetAccess = requestVK config getKeyAccessUrl,
              doGetMessage = \k s ts -> requestVK config (getMessageUrl k s ts),
              doSendMessage = \id text btn ->
                           requestVK config  (sendMessageUrl id text btn), 
              doGetAnswerForSend = \id text btn -> 
                           answerCreator  id text btn listUser         
            },
            logger = Logger $ logMessage l
          }
  runReaderT requestAccess app
  logStop l

requestAccess ::
  Log m =>
  Requestable m =>
  m Message
requestAccess = do
  logI " get access "
  resp <- getAccess
  case resp of
    Access access -> do
      logI " accessed, awaiting messages "
      logI $ key access
      getMessages
        (BS8.pack $ key access)
        (BS8.pack $ server access)
        (BS8.pack $ ts access)
    ErrorVK error -> do
      logI $ error_msg error
      pure NoMessage
    _ -> do
      logI $ pack . show $ resp
      pure NoMessage

getMessages ::
  Log m =>
  Requestable m =>
  BS8.ByteString ->
  BS8.ByteString ->
  BS8.ByteString ->
  m Message
getMessages key server ts = do
  resp <- getMessage key server ts
  case resp of
    Message' ts' updates ->
      if not (null updates)
        then do
          mapM_
            ( \MessageUpdates {..} ->
                case _type of
                  "message_new" -> do
                    logI $ "event: " <> _type
                    logI $ show resp
                    logI $
                      "received message from user id :"
                        <> show from_id
                        <> " "
                        <> text
                        <> "button :"
                        <> _payload
                    postMessage from_id text  _payload
                  _ -> logI $ pack "event: " <> pack _type
            )
            updates
          logI " awaiting messages "
          getMessages key server (BS8.pack ts')
        else do
          logI " awaiting messages "
          getMessages key server ts
    _ -> do
      logI $ pack . show $ resp
      pure NoMessage

postMessage ::
  Log m =>
  Requestable m =>
  Integer ->
  String ->
  String ->
  m ()
postMessage id text btn = do
  logI $ "send message to user id:" <> show id <> " " <> text
  let btn' = if btn /= "" then read btn else 0 
  answer <- getAnswerForSend id text btn' 
  resp <- uncurry (sendMessage (BS8.pack . show $ id)) answer
  case resp of
    Response rsp -> do
      logI $ "recponse code " <> show rsp
    _ -> do
      logI $ pack . show $ resp

data Application m = Application
  { logger :: Logger m,
    requests :: Requests m
  }
  deriving stock (Generic)

instance Has (Logger m) (Application m) where
  getter = logger
  modifier f a = a {logger = f . logger $ a}

instance Has (Requests m) (Application m) where
  getter = requests
  modifier f a = a {requests = f . requests $ a}
