{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
import           Config               (Subd (..), Config (..), LogOpts (..),
                                       readConfig1,  )
import Exception
import           Log                  (Log (..), Logger (..), Priority (..),
                                       logLn, 
                                        )
import Data.Has (Has, getter,modifier)
import           GHC.Generics         (Generic)

import           Control.Monad.Reader (runReaderT)
import Data.Text (unpack, pack, Text)
import           Data.Time.Format     (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime  (getZonedTime)
import           GHC.Exception        (prettyCallStack)
import           GHC.Stack            (HasCallStack, callStack)
import Control.Exception.Base (catch, SomeException)




{--
app   =   Application
                  { 
                    logger = Logger   {
                                        dologLn =
                                        \LogOpts{..}  a -> do
                                        appendFile (show nameLogInfo) $  unpack a
                                        putStrLn $  unpack a
                                        putStrLn (prettyCallStack callStack)
                    
                  }
                  }
--}

newtype LogOpt
  =LogOpt
    { 
      nameLog :: String
    }      
  
logOpt = LogOpt {nameLog = "log.log"}
logX  str = appendFile  (nameLog logOpt) str
main :: IO ()
main =do
      conf <- readConfig1 
      case conf of 
        Left err -> putStrLn err
        Right Config{..} ->do                                 --print $ pathToLog (logOpts)
         let  app = Application
               { 
                 logger = 
                     Logger {
                              dologLn = \a -> do
                                 catch (appendFile (pathToLog (logOpts) ++ nameLogInfo(logOpts))  $  unpack a) handler 
                        }
               }
         runReaderT (logLn $ pack "Hi") app
          where
            handler :: SomeException -> IO ()
            handler ex = putStrLn $ "Caught exception: " ++ show ex                          
                 

data Application m = Application
 {
  logger ::Logger m
 }
   deriving stock (Generic)


instance Has (Logger m) (Application m ) where
 getter = logger
 modifier f a = a { logger = f . logger $ a }
 
