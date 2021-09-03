{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Network.Class

where

import Network.Types
import Network.App
import Network.ErrorTypes
import Data.Has (Has, getter)
import Control.Monad.Reader (ReaderT, asks, lift, runReaderT)
import           Logger.Class           (Log (..))
import qualified Data.ByteString.Char8 as BS8
import Control.Monad.State.Lazy (StateT)
import Control.Monad.RWS.Lazy (RWST)

   

newtype DoRequest m = DoRequest  {doRequest ::   Url ->  m Message}

class Monad m => Requestable  m where
 request  :: Url ->  m Message

instance
  ( Has (DoRequest m) r,
    Show w,
    Monoid w,
    Monad m
  ) =>
   Requestable  (RWST r w s m)    where
   request  url  =  asks getter >>= \(DoRequest doReq) -> lift $ doReq  url 
  