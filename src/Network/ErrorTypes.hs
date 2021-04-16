{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module Network.ErrorTypes where

import GHC.Generics
import Data.Aeson.Types (FromJSON)
import Data.String


data Err= Err 
 { error_code :: Int,
   error_msg :: String 
 } | GetKeyTsErr 
 { failed :: Int,
   ts :: Maybe Int
 }
 deriving (Generic, FromJSON,  Show)

newtype ErrorVK
  = ErrorVK {error :: Err}
  deriving (Generic, FromJSON, Show ) 
 

