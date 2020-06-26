{-# LANGUAGE BlockArguments #-}

module Utility (
 
)where


import Data.Aeson.Types (Object, parseMaybe )
import qualified Data.ByteString as B
import Control.Exception.Base (SomeException, try)
import Data.Aeson (decodeStrict)
import System.Environment.Blank (getArgs)

