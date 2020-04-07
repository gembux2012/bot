module Main where

import Logger
import qualified Data.ByteString as B
import           Control.Monad (mzero)
import           Data.Aeson


main = do
   --logI "todo.txt" "bot start"
  
  rawJSON <- B.readFile "config.json"
      
  let result = decodeStrict rawJSON :: Maybe Object  
  putStrLn $ case result of
         Nothing         -> "Invalid JSON!"
         Just emailsList -> show emailsList