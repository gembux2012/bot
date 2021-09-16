{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Network.Class where

import           Control.Monad.Reader     (ReaderT, asks, lift, runReaderT, Reader)
import           Control.Monad.RWS.Lazy   (RWST, MonadIO, ask)
import           Control.Monad.State.Lazy (StateT)
import qualified Data.ByteString.Char8    as BS8
import           Data.Has                 (Has, getter)
import           Logger.Class             (Log (..))
import           Network.App

import           Network.Types

import Logger.Types


data Requests m = Requests
  { doGetAccess ::  m Message,
    doGetMessage :: BS8.ByteString -> BS8.ByteString -> BS8.ByteString -> m Message,
    doSendMessage :: BS8.ByteString -> BS8.ByteString -> BS8.ByteString -> m Message,
    doGetAnswerForSend ::  Integer -> String -> Int -> m (BS8.ByteString,BS8.ByteString)
    
  }

class Monad m  => Requestable m where
  getAccess ::  m Message
  getMessage :: BS8.ByteString -> BS8.ByteString -> BS8.ByteString -> m Message
  sendMessage :: BS8.ByteString -> BS8.ByteString -> BS8.ByteString -> m Message
  getAnswerForSend :: Integer -> String -> Int -> m (BS8.ByteString, BS8.ByteString)

instance
  ( Has (Requests m) r,
    Monad m,
    MonadIO m
  ) =>
  Requestable (ReaderT r m)
  where
  getAccess  = asks getter >>= \(Requests doGetAcc _ _ _) -> lift $ doGetAcc 
  getMessage k s ts = asks getter >>= \(Requests _ doGetMess _ _) -> lift $ doGetMess k s ts
  sendMessage id text btn = asks getter >>= \(Requests _ _ doSendMess _) -> lift $ doSendMess id text btn
  getAnswerForSend  id text btn = asks getter >>= \(Requests _ _ _ doGAFS) -> lift $ doGAFS  id text btn
  
instance  Requestable (ReaderT VKOpts IO) -- (ReaderT Config m) 
 where
  getAccess  =  ask >>= \c ->  requestVK c  getKeyAccessUrl
  getMessage k s ts = ask >>= \c  -> requestVK c (getMessageUrl k s ts)
  sendMessage id text btn = ask >>= \c  -> requestVK c (sendMessageUrl id text btn)    