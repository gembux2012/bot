{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Answer where

import qualified Data.ByteString.Char8    as BS8
import           Network.Types

import Logger.Types
import Data.Has (Has, getter)
import Control.Monad.Reader (ReaderT, asks, lift)


newtype Answer m = Answer
  { 
    doGetAnswerForSend ::  Integer -> String -> Int -> m (BS8.ByteString,BS8.ByteString)
    
  }

class Monad m  => Answerer m where
  getAnswerForSend :: Integer -> String -> Int -> m (BS8.ByteString, BS8.ByteString)

instance
  ( Has (Answer m) r,
    Monad m
  ) =>
  Answerer (ReaderT r m)
  where
  getAnswerForSend  id text btn = asks getter >>= \(Answer doGAFS) -> lift $ doGAFS  id text btn
  

