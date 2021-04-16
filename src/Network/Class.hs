{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Class

where

import Network.Types
import Network.ErrorTypes
import Data.Has (Has, getter)
import Control.Monad.Reader (ReaderT, asks, lift)

class Monad m => GetMessageVK m where
 request  :: Method -> Url ->  m (Either ErrorVK Message)
 
newtype DoRequest m = DoRequest  {doRequest ::  Method -> Url ->  m (Either ErrorVK Message)}

instance
  ( Has (DoRequest m) r,
    Monad m
  ) =>
  GetMessageVK (ReaderT r m)   where 
   request m u  = asks getter >>= \(DoRequest doReq) -> lift $ doReq m u  