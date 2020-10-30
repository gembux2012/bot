{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Log
   ( Loggable(..)
    , Log(..)
    , Logger(..)
    , Priority(..)
    , logLn
    ,logLnInit
    , defaultLogOpts

    )  where

import           Prelude

import           Config (LogOpts(..), Config,  logOpts )                    
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
  logLn :: HasCallStack =>Loggable a => LogOpts->a -> m ()
  logLnInit ::  LogOpts -> ReaderT r m () 
  

data Priority =  INFO | NOTICE | WARNING | ERROR deriving (Show)



data Logger m = Logger
  { dologLn :: HasCallStack =>LogOpts-> Text -> m ()
   
    -- 
  }
  
defaultLogOpts = LogOpts 
           { pathToLog = ""
            ,maxSizeLog = 1
            ,nameLogInfo = "bot.log"
            ,showToConsole =1 
            }  

instance
  ( Has (Logger m) r
  , Monad m
  
  ) => Log (ReaderT r m) where
  --logLnInit c  =
    --asks getter >>= \(Logger doLog ) -> lift $ doLog c   
                                                                      
  logLn logOpt a = do
    (Logger  doLog  ) <- asks getter
    lift $ doLog  logOpt  (fromLoggable a) 
  
 

instance Loggable Text  where
  fromLoggable = id

instance Loggable Priority where
  fromLoggable =  pack.show
  

