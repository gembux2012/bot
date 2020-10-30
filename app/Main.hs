{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
import           Config               (Base (..), Config (..), LogOpts (..),
                                       readConfig1,  )
import Exception
import           Log                  (Log (..), Logger (..), Priority (..),
                                       logLn,logLnInit,defaultLogOpts


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
                  { 
                    logger = Logger   {
                                        dologLn =
                                        \LogOpts{..}  a -> do
                                        appendFile (show nameLogInfo) $  unpack a
                                        putStrLn $  unpack a
                                        putStrLn (prettyCallStack callStack)
                    
                  }
                  }


  

main :: IO ()
main =  runReaderT (api "jkl")  app
       


api str  = do
   conf <- readConfig1 
   case conf of
    Left  error -> defaultApp  error
    Right conf  -> initApp conf  "ok"
 
initApp Config{..} str = logLn logOpts str 
defaultApp str = logLn defaultLogOpts str 
               


data Application m = Application
 {
   config ::  Config
  ,logger ::Logger m
 }
   deriving stock (Generic)


instance Has (Logger m) (Application m ) where
 getter = logger
 modifier f a = a { logger = f . logger $ a }
 
