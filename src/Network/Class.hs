{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Class
( DoRequest(..),
  RequestSN(..)
)
where

import Network.Types
import Data.Has (Has, getter)
import Control.Monad.Reader (ReaderT, asks, lift)

class Monad m => RequestSN m where
 auth ::  m ResponseMessage
 
newtype DoRequest m = DoRequest  {doRequest :: m ResponseMessage}

instance
  ( Has (DoRequest m) r,
    Monad m
  ) =>
  RequestSN (ReaderT r m)   where 
   auth = asks getter >>= \(DoRequest doReq) -> lift doReq 