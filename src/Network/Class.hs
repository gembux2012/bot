{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Class
( Requestable(..),
  Req(..),
 
)
where

import Network.Types
import Data.Has (Has, getter)
import Control.Monad.Reader (ReaderT, asks, lift)

class Monad m => Req m where
 request :: Method -> Url -> m ResponseMessage
 
newtype Requestable m = Requestable  {doRequest :: Method -> Url -> m ResponseMessage}

instance
  ( Has (Requestable m) r,
    Monad m
  ) =>
  Req (ReaderT r m)   where 
   request m url  = asks getter >>= \(Requestable request) -> lift $ request m url