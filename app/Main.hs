module Main where
import Logger
import Config
import Exception

import Control.Exception.Base (catch)      

main = do
       config <- readConfig
       putStrLn $ case config of
                  Left str  ->  str 
                  Right cfg -> show cfg
     
        
     -- logI "todo.txt" "bot start"
   

  
{-   
fromList [("ip",String "127.0.0.1"),("social",Object
            (fromList [("GitHub",Object 
               (fromList [("blog",String "https://github.com/denisshevchenko/blog"),
                  ("ohaskell",String "https://github.com/denisshevchenko/ohaskell")]))])),
                        ("anybody",String "iouoiuio"),("port",Number 3000.0)]                           
  -}                               
