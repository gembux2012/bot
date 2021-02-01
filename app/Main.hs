{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import           Config                 (Config (..), LogOpts (..), Subd (..),
                                         readConfig1)
import           Control.Exception.Base (SomeException, catch, fromException,
                                         try)
import           Control.Monad.Reader   (runReaderT)
import           Data.Has               (Has, getter, modifier)
import           Data.Maybe
import           Data.Text              (Text, append, pack, unpack)
import           Data.Time.Format       (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime    (getZonedTime)
import           Exception
import           GHC.Exception          (prettyCallStack)
import           GHC.Generics           (Generic)
import           GHC.Stack              (HasCallStack, callStack)
import           Log                    (Log (..), Logger (..), Priority (..),
                                         logLn, printLog)

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
appendFile' path str = catch (appendFile path str) handler
  where
    handler :: SomeException -> IO ()
    handler ex = putStrLn $ "Caught exception: " ++ show ex

newtype LogOpt = LogOpt
  { nameLog :: String
  }

logOpt = LogOpt {nameLog = "log.log"}

logX str = appendFile (nameLog logOpt) str

main :: IO ()
main = do
  conf <- readConfig1
  case conf of
    Left err -> putStrLn err
    Right Config {..} -> do
      
      let app = Application
            { logger = Logger
              { {---dologLn = \a -> do
                  let appendFile' path str = try (appendFile path str) :: IO (Either SomeException ())
                  result <- appendFile' (pathToLog (logOpts) ++ nameLogInfo (logOpts)) $ unpack a ++ "\n"
                  case result of
                   Left ex -> putStrLn $ "Caught exception: " ++ show ex
                   Right val -> return () --}
                   dologLn = \a -> printLog (pathToLog (logOpts))  (nameLogInfo (logOpts)) $ unpack a ++ "\n"  
              }
            }
      --runReaderT (logLn $ pack "logger initialized") app
      runReaderT (logLn $ pack "bot start") app


data Application m = Application
  { logger :: Logger m
  }
  deriving stock (Generic)

instance Has (Logger m) (Application m) where
  getter = logger
  modifier f a = a {logger = f . logger $ a}
