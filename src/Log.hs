{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Log
  ( Loggable (..),
    Log (..),
    Logger (..),
    Priority (..),
    logLn,
    
    --defaultLogOpts,
    SetOpts,
  )
where

import Config (Config, LogOpts (..), logOpts)
import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.Reader.Class (asks)
import Data.Has (Has (..))
import Data.Text (Text, append, pack, unpack)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime, ZonedTime, getZonedTime)
import GHC.Stack (HasCallStack)
import Prelude

class Loggable a where
  fromLoggable :: a -> Text

class Monad m => Log m where
  logLn :: HasCallStack => Loggable a => a -> m ()
  
data Logger m = Logger
  { dologLn :: HasCallStack => Text -> m ()
  }
  

--logLnInit ::  LogOpts ;

data Priority = INFO | NOTICE | WARNING | ERROR deriving (Show)

newtype SetOpts m = SetOpts {logOpt :: LogOpts -> Text -> m ()}


{--
defaultLogOpts =
  LogOpts
    { pathToLog = "kjk",
      nameLogInfo = "bot.log"
     
    }
--}




instance
  ( Has (Logger m) r,
    Monad m
  ) =>
  Log (ReaderT r m)
  where
   logLn a =
      asks getter >>= \(Logger doLog) -> lift . doLog . fromLoggable $ a
      
instance Loggable Text where
  fromLoggable = id

--instance Loggable Priority where
--  fromLoggable = pack . show
