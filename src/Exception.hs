module Exception
(
  fileException
 ) where
 
import Control.Exception
import System.IO.Error (isDoesNotExistError,isPermissionError,isFullError,ioeGetFileName )

fileException :: IOException -> IO ()
fileException e
    | isDoesNotExistError e = case ioeGetFileName e of
                              Just fileName -> putStrLn $ "fail " ++ fileName ++  " not  exists!"
                              Nothing -> putStrLn "file not exists!"
    | isPermissionError e = putStrLn "access denied!"
    | isFullError e = putStrLn "There is not enough diskspace available!"
    | otherwise = ioError e
    where fileName = ioeGetFileName e