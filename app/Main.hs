{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE OverloadedStrings #-}

module Main where
import Config (Log(..),Config(..),readConfig1)
--import Exception
import Log (Log(..), Logger(..), Priority(..),logLnI)
import Data.Has (Has, getter,modifier)
import GHC.Generics (Generic)

import Control.Monad.Reader (runReaderT)
import Data.Text (unpack, pack, Text)
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import GHC.Exception (prettyCallStack)
import GHC.Stack (callStack, HasCallStack)


appInit conf = case conf of
                       --Left  error -> logLnI $ pack error
                       Right (Config config) -> app config
  
app conf =   Application
       { logger = Logger  {dologLn   = \a -> getZonedTime >>= \t -> do  
                          --case conf of
                            let logFileName =pathToLog (Log conf)
                            --Just (Config (Log pathToLog ))->    
                            appendFile logFileName ++ "/bot.log" $ formatTime defaultTimeLocale "%m-%d-%Y %H:%M:%S %Z" t ++ " " ++ unpack a
                            putStrLn (prettyCallStack callStack)}
         }          

main :: IO ()
main = readConfig1 >>= \configExistst ->appInit configExistst
--readConfig1 >>= \configExistst ->  runReaderT (api configExistst ) app
  --where
                                           
api config = case config of
        Left  error -> logLnI $ pack error
       
             
      

data Application m = Application
 {  logger ::Config config=> Logger m
 } 
   --deriving stock (Generic)
 

instance Has (Logger m) (Application m) where
 getter = logger
 modifier f a = a { logger = f . logger $ a }



                
                