{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import           Config                 (Config (..), readConfig)
import           Control.Exception.Base ()
import           Control.Monad.Reader   (runReaderT, MonadIO)
import           Data.Has               (Has, getter, modifier)
import           GHC.Generics           (Generic)
import           Logger.App             (printLog)

import           Logger.Adt             (Logger(..), )
import           Logger.Class           (Log (..))
import Data.Text (Text, pack)
--import Data.ByteString.Char8 (pack)
import Network.Api (run)
import Control.Monad.Catch.Pure (MonadThrow)
import Control.Monad.Base (MonadBase)
import Control.Concurrent

main :: IO ()
main = do
  conf <- readConfig
  m <- newEmptyMVar
  putStrLn  $ fst conf
  let Config{..} = snd conf
  forkIO (dologLn logOpts)  
   
  let app = Application
            { logger = Logger
              {
                dologLn  = \msg-> putMVar(m msg)
                              
              }
            }
  runReaderT api  app
  
  return () 
  where
     loop m = do
       cmd <- takeMVar m
       printLog cmd
       loop     --printLog logOpts
  
  
api :: 
   MonadThrow m
    => MonadIO m
    =>MonadBase IO m
    => Log m 
   => m ()
api = do  
 logI "bot start"
 _ <- run
 return()

 
 

newtype Application m
  = Application {logger :: Logger m}
  deriving stock Generic

instance Has (Logger m) (Application m) where
  getter = logger
  modifier f a = a {logger = f . logger $ a}

