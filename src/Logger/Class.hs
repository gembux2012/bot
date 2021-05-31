{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Logger.Class
  ( Log (..),
  )
where

import Control.Monad.Reader (ReaderT, asks, lift)
import Data.Has (Has, getter)
import Data.Text (Text, append, pack)
import GHC.Stack.Types (HasCallStack)
import Logger.Adt (Priority (..))



data Logger m = Logger
  { dologLn :: HasCallStack =>  Text -> m ()
  }
  
class Loggable a where
  fromLoggable :: a -> Text

class Monad m => Log m where
  logI :: HasCallStack => Loggable a => a -> m ()
  logW :: HasCallStack => Loggable a => a -> m ()
  logE :: HasCallStack => Loggable a => a -> m ()
  
  

instance
  ( Has (Logger m) r,
    Monad m
  ) =>
  Log (ReaderT r m)
  where
  logI  a =
    asks getter >>= \(Logger doLog) -> lift $ doLog ( fromLoggable INFO <> " " <>  fromLoggable a)
  logW a =
    asks getter >>= \(Logger doLog) -> lift . doLog $ fromLoggable WARNING <> " " <>  fromLoggable a
  logE a =
    asks getter >>= \(Logger doLog) -> lift . doLog $ fromLoggable ERROR <> " " <> fromLoggable a

instance Loggable Text where
  fromLoggable = id

instance Loggable Priority  where
 fromLoggable = pack.show
  