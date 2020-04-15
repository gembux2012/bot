module Exception
(
  ConfigError(..)
 ) where
 
import Control.Exception
--import System.IO.Error (isDoesNotExistError,isPermissionError,isFullError,ioeGetFileName )
import qualified Data.ByteString as B
import Data.Data (Typeable)

data  ConfigError = ConfigError String deriving (Show, Typeable)
instance Exception ConfigError

{-
fileException :: IOException -> IO  ()
fileException e 
    | isDoesNotExistError e = case ioeGetFileName e of
                              Just fileName -> putStrLn $ "fail " ++ fileName ++  " not  exists!"
                              Nothing -> putStrLn  "file not exists!" 
    | isPermissionError e = putStrLn "access denied!"
    | isFullError e = putStrLn "There is not enough diskspace available!"
    | otherwise = ioError e
    where fileName = ioeGetFileName e
    -}