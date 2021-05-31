{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings     #-}


module Network.Class

where

import Network.Types
import Network.Api
import Network.ErrorTypes
import Data.Has (Has, getter)
import Control.Monad.Reader (ReaderT, asks, lift)
import           Logger.Class           (Log (..))
import qualified Data.ByteString.Char8 as BS8

   

newtype DoRequest m = DoRequest  {doRequest ::   Url ->  m (Either ErrorVK Message)}

class Monad m => Requestable m where
 request  ::  Url ->  m (Either ErrorVK Message)
 


instance
  ( Has (DoRequest m) r,
    Monad m
  ) =>
  Requestable (ReaderT r m)   where
   request  url  = asks getter >>= \(DoRequest doReq) -> lift $ doReq url