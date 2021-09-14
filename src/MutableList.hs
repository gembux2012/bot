{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MutableList where

import Data.Map.Internal (Map)
import Control.Concurrent.Lifted (MVar, newMVar, takeMVar, putMVar)
import qualified Data.Map as Map


class ( Ord k ) => MutableList k a where
 newList ::  IO (MVar(Map k a))      
 newList =  newMVar Map.empty        
 
 insert :: MVar(Map  k a) -> k -> a -> IO ()
 insert m k a = do 
   list <- takeMVar m
   putMVar m (Map.insert k a list)
   
   
 lookup :: MVar(Map  k a) -> k -> IO (Maybe a)
 lookup  m k = do
   list <- takeMVar m
   putMVar m list
   return (Map.lookup k list)      
 
 update :: MVar(Map  k a) -> k -> a -> IO ()
 update  m k a = do
   list <- takeMVar m
   putMVar m (Map.update (const $ Just a ) k list)   
 
 getList :: MVar(Map  k a) -> IO(Map  k a)
 getList  m = do takeMVar m
     

instance (Ord k ) => MutableList k a where


