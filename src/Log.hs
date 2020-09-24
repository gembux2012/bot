{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Log
  ( Loggable(..)
  , Log(..)
  , Logger(..)
  , Priority(..)
  , logLnI
  
  ) where

import Prelude

import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.Reader.Class (asks)
import Data.Has (Has(..))
import Data.Text (Text, pack, append, unpack)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Data.Time (getZonedTime, formatTime,defaultTimeLocale, ZonedTime)
import Prelude ((++))

data Priority =  INFO | NOTICE | WARNING | ERROR deriving (Show)

class Loggable a where
  fromLoggable :: a -> Text
  
class Monad m => Log m where
  logLn :: HasCallStack => Loggable a  => Priority -> a -> m ()
  

data Logger m = Logger
  { 
   dologLn :: HasCallStack => Text -> m ()
   
  }
  

                                            

instance
  ( Has (Logger m) r
  , Monad m
  ) => Log (ReaderT r m) where
  
  logLn pr a = do
    asks getter >>= \(Logger doLog  ) ->lift.doLog $ 
                    (
                      fromLoggable pr 
                      `append`  " "
                      `append` fromLoggable a )
  
logLnI a = logLn INFO a  


     
 
                     
                    
instance Loggable Text where
  fromLoggable = id
  
instance Loggable Priority where
   fromLoggable = pack.show





