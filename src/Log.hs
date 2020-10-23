{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Log
   ( Loggable(..)
    , Log(..)
    , Logger(..)
    , Priority(..)
    , logLn
    ,logLnInit
    , logLn

    )  where

import           Prelude

import           Config (LogOpts(..), Config )                    
import           Control.Monad.Reader       (ReaderT, lift)
import           Control.Monad.Reader.Class (asks)
import           Data.Has                   (Has (..))
import           Data.Text                  (Text, append, pack, unpack)
import           GHC.Stack                  (HasCallStack)
import Data.Time.LocalTime (getZonedTime, LocalTime, ZonedTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

class Loggable a where
  fromLoggable :: a -> Text

class Monad m => Log m where
  logLn :: HasCallStack => Loggable a => a -> m ()
  logLnInit ::  LogOpts -> ReaderT r m () 
  

data Priority =  INFO | NOTICE | WARNING | ERROR deriving (Show)



data Logger m = Logger
  { dologLn :: HasCallStack =>LogOpts-> Text -> m ()
   ,logOpt :: LogOpts    
    -- 
  }

instance
  ( Has (Logger m) r
  , Monad m
  ) => Log (ReaderT r m) where
  --logLnInit c  =
    --asks getter >>= \(Logger doLog ) -> lift $ doLog c   
                                                                      
  logLn  a = do
    (Logger  doLog logOpt ) <- asks getter
    lift $ doLog logOpt  (fromLoggable  a) 
  
 

instance Loggable Text  where
  fromLoggable = id

instance Loggable Priority where
  fromLoggable =  pack.show
  


