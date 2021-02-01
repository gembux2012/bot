{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Log
  ( Loggable (..),
    Log (..),
    Logger (..),
    Priority (..),
    --logLn,
    
    --defaultLogOpts,
    SetOpts,
    printLog
  )
where

import Config (Config, LogOpts (..), logOpts, )
import Control.Monad.Reader (ReaderT, lift, void)
import Control.Monad.Reader.Class (asks)
import Data.Has (Has (..))
import Data.Text (Text, append, pack, unpack)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime, ZonedTime, getZonedTime)
import GHC.Stack (HasCallStack)
import Prelude
import Control.Exception.Base (SomeException, try)

class Loggable a where
  fromLoggable :: a -> Text

class Monad m => Log m where
  logLn :: HasCallStack => Loggable a => a -> m ()

data Logger m = Logger
  { 
    dologLn :: HasCallStack => Text -> m ()
  }


--logLnInit ::  LogOpts ;

data Priority = INFO | NOTICE | WARNING | ERROR deriving (Show)

newtype SetOpts m = SetOpts {logOpt :: LogOpts -> Text -> m ()}


{--
defaultLogOpts =
  LogOpts
    { pathToLog = "kjk",
      nameLogInfo = "bot.log"

    }
--}




instance
  ( Has (Logger m) r,
    Monad m
  ) =>
  Log (ReaderT r m)
  where
   logLn a =
      asks getter >>= \(Logger  doLog) -> lift . doLog . fromLoggable $ a

instance Loggable Text where
  fromLoggable = id

--instance Loggable Priority where
--  fromLoggable = pack . show

printLogX :: [Char] -> [Char] -> String ->Int-> IO ()
printLogX _ _ _ 0 = return() 
printLogX path fName str n    =  do
   result <- try (appendFile (path ++ fName) str )  :: IO (Either SomeException ())
   case result of
    Right val -> return ()
    Left ex -> do 
     if n == 2 then do  
       printLogX "" fName (show ex <> ": log will be written to the current directory") (n-1)
       else do
        putStrLn (show ex <> ": cannot write a log, the app will be stopped") 
        printLogX "" "" "" (n-1)
        
printLog path fName str = printLogX path fName str 2        