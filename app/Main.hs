{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where
import           Config               (Base (..), Config (..), LogOpts (..),
                                       readConfig1)
import Exception
import           Log                  (Log (..), Logger (..), Priority (..),
                                       logLn,logLnInit,


                                        )
import Data.Has (Has, getter,modifier)
import           GHC.Generics         (Generic)

import           Control.Monad.Reader (runReaderT)
import Data.Text (unpack, pack, Text)
import           Data.Time.Format     (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime  (getZonedTime)
import           GHC.Exception        (prettyCallStack)
import           GHC.Stack            (HasCallStack, callStack)




app   =   Application
                  { config= do config <- readConfig1
                    ,
                    logger = Logger   {
                                        dologLn =
                                        \logOpt  a -> do
                                        let  logFileName =  nameLogInfo  logOpt
                                        appendFile (show logFileName) $  unpack a
                                        putStrLn $  unpack a
                                        putStrLn (prettyCallStack callStack)
                                       , logOpt =logLn (logOpts config ) 
                                      }
                  }


  

main :: IO ()
main =do 
        config <- readConfig1
        case config of
                Left  error -> log'  error
                Right conf  -> let    
       
logLni c a = runReaderT (logLn (logOpts c) a) app
logL c =logLni c
log' a = logL a                  
--logLn:: Log m =>Either String Config ->(Either String LogOpts)

--logLn' = logLn $ logOpts conf

--api :: Log.Log m => Either String Config -> m ()

api config = case config of
        Left  error ->logLn  error
        --Right (Config (Log nameLogInf _ _ _ ) _ )   -> logLnI $ pack (show nameLogInf )
        Right conf  ->logLn (logOpts conf) 
             


data Application m = Application
 {
   logger ::Logger m
  

 }
   deriving stock (Generic)


instance Has (Logger m) (Application m ) where
 getter = logger
 modifier f a = a { logger = f . logger $ a }
 
