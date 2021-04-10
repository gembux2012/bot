{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Class
( DoRequest(..),
  GetMessageVK (..)
  
)
where

import Network.Types
import Data.Has (Has, getter)
import Control.Monad.Reader (ReaderT, asks, lift)

class Monad m => GetMessageVK m where
 request  :: Method -> Url ->  m ResponseMessage
 
newtype DoRequest m = DoRequest  {doRequest :: Method -> Url ->  m ResponseMessage}

instance
  ( Has (DoRequest m) r,
    Monad m
  ) =>
  GetMessageVK (ReaderT r m)   where 
   request m u  = asks getter >>= \(DoRequest doReq) -> lift $ doReq m u  