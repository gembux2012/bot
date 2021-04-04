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
import Data.Text (Text, pack, unpack)
--import Data.ByteString.Char8 (pack)
import Network.Api 
import Control.Monad.Catch.Pure (MonadThrow)
import Control.Monad.Base (MonadBase)
import Control.Concurrent

import Network.Class (Requestable(..),request,Req)
import Network.Types (ResponseMessage(..),Method(..),Url(..))
import Network.Api ()


data LogCommand = MessageL Text | Stop (MVar ())


main :: IO ()
main = do
  conf <- readConfig
  m <- newEmptyMVar
  let config = snd conf
  putStrLn  $ fst conf
  let app = Application
            { logger = Logger 
              {
                dologLn  = putMVar m . MessageL
              },
              do_request = Requestable
              {
                doRequest =
                 requestVK 
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
     MessageL msg -> do
       printLog  (logOpts conf)  msg 
       loop      
     Stop s -> do
          putMVar s ()
{--
api :: 
   MonadThrow m
    => MonadIO m
    -- =>MonadBase IO m
   => Log m 
   => m ()
--}

api ::
 Log m =>
 Req m =>  m () 
api = do 
 logI "start bot"
 _ <- runBot GetKeyAccess getKeyAccessUrl 
 pure()
 
runBot m url = do 
 resp <- request m url  
 case  resp  of
  Error err -> do 
      logE $  pack err
      pure()
  Auth (secKey,ts) -> do
   logI $ pack (show secKey ++ " " ++ show ts)
   runBot  GetMessage (getMessageUrl secKey ts)
   
 

data  Application m   = Application 
  {logger :: Logger m ,
   do_request :: Requestable m 
  }
  deriving stock Generic

instance Has (Logger m) (Application m) where
  getter = logger
  modifier f a = a {logger = f . logger $ a}

