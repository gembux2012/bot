{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module Network.ErrorTypes where

import GHC.Generics
import Data.Aeson.Types 
import Data.String


data Err= Err 
 { error_code :: Int,
   error_msg :: String 
 } 
 deriving (Generic, FromJSON,  Show)

newtype ErrorVK
  = ErrorVK {error :: Err}
  deriving (Generic, FromJSON, Show )
 

