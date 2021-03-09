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

data LogCommand = Message Text | Stop (MVar ())


main :: IO ()
main = do
  conf <- readConfig
  m <- newEmptyMVar
  let config = snd conf
  putStrLn  $ fst conf
  let app = Application
            { logger = Logger
              {
                dologLn  = putMVar m . Message
                              
              }
            }
  forkIO $ logger' config m
  runReaderT api  app
 
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s             

 
   
logger' :: Config -> MVar LogCommand -> IO ()
logger' conf m  = loop 
 where  
   loop   = do
    cmd <- takeMVar m
    case cmd of
     Message msg -> do
       printLog  (logOpts conf)  msg 
       loop      
     Stop s -> do
          putMVar s ()

api :: 
   MonadThrow m
    => MonadIO m
    =>MonadBase IO m
    => Log m 
   => m ()
api = do  
 logI "bot start"
 _ <- run
 return ()

 
 

newtype Application m
  = Application {logger :: Logger m}
  deriving stock Generic

instance Has (Logger m) (Application m) where
  getter = logger
  modifier f a = a {logger = f . logger $ a}

