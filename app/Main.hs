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

main :: IO ()
main = do
  conf <- readConfig
  putStrLn  $ fst conf
  let Config{..} = snd conf   
  let app = Application
            { logger = Logger
              {
                dologLn  = printLog logOpts    
              }
            }
  runReaderT api  app
  
  return () 
  
  
api :: 
   MonadThrow m
    => MonadIO m
    =>MonadBase IO m
    => Log m 
  => Log m =>
   m ()
api = do  
 logE "bot start"
 _ <- run
 return()

 
 

newtype Application m
  = Application {logger :: Logger m}
  deriving stock Generic

instance Has (Logger m) (Application m) where
  getter = logger
  modifier f a = a {logger = f . logger $ a}

