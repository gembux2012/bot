{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Class where

import           Control.Monad.Reader     (ReaderT, asks, lift, runReaderT)
import           Control.Monad.RWS.Lazy   (RWST)
import           Control.Monad.State.Lazy (StateT)
import qualified Data.ByteString.Char8    as BS8
import           Data.Has                 (Has, getter)
import           Logger.Class             (Log (..))
import           Network.App
import           Network.ErrorTypes
import           Network.Types

data Requests m = Requests
  { doGetAccess :: m Message,
    doGetMessage :: BS8.ByteString -> BS8.ByteString -> BS8.ByteString -> m Message,
    doSendMessage :: BS8.ByteString -> String -> String -> m Message
  }

class Monad m => Requestable m where
  getAccess :: m Message
  getMessage :: BS8.ByteString -> BS8.ByteString -> BS8.ByteString -> m Message
  sendMessage :: BS8.ByteString -> String -> String -> m Message

instance
  ( Has (Requests m) r,
    Monad m
  ) =>
  Requestable (ReaderT r m)
  where
  getAccess = asks getter >>= \(Requests doReq _ _) -> lift doReq
  getMessage k s ts = asks getter >>= \(Requests _ doGetMess _) -> lift $ doGetMess k s ts
  sendMessage id text btn = asks getter >>= \(Requests _ _ doSendMess) -> lift $ doSendMess id text btn
