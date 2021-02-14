{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Logger.Class
  ( Log (..),
  )
where

import Control.Monad.Reader (ReaderT, asks, lift)
import Data.Has (Has, getter)
import Data.Text (Text, append, pack)
import GHC.Stack.Types (HasCallStack)
import Logger.Adt (Logger (..), Priority (..))

class Monad m => Log m where
  logI :: HasCallStack => Text -> m ()
  logW :: HasCallStack => Text -> m ()
  logE :: HasCallStack => Text -> m ()

instance
  ( Has (Logger m) r,
    Monad m
  ) =>
  Log (ReaderT r m)
  where
  logI t =
    asks getter >>= \(Logger doLog) -> lift . doLog $ (pack . show $ INFO) `append` " " `append` t
  logW t =
    asks getter >>= \(Logger doLog) -> lift . doLog $ (pack . show $ WARNING) `append` " " `append` t
  logE t =
    asks getter >>= \(Logger doLog) -> lift . doLog $ (pack . show $ ERROR) `append` " " `append` t
