module Streams where

import Control.Concurrent.Lifted (putMVar,takeMVar,newEmptyMVar,MVar)
import Control.Concurrent (forkIO)

import Config
import Logger.App (printLog)
import Data.Text (Text)


newtype Logger' = Logger' (MVar LogCommand)
data LogCommand = Message Text | Stop (MVar ())  

initLogger :: Config -> IO Logger'
initLogger conf = do
  m <- newEmptyMVar
  let l = Logger' m
  forkIO  (logger' conf l)
  return l 


logMessage :: Logger' -> Text -> IO ()
logMessage (Logger' m) s = putMVar m (Message s)

logStop :: Logger' -> IO ()
logStop (Logger' m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s

logger' ::Config -> Logger' -> IO ()
logger' conf (Logger' m) = loop
  where
    loop = do
      cmd <- takeMVar m
      case cmd of
        Message msg -> do
          printLog (logOpts conf) msg
          loop
        Stop s -> do
          putStrLn "logger: stop"
          putMVar s ()