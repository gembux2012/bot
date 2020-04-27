module Main where
import Logger
import Config
import Exception

import Control.Exception.Base (catch)      

getSetting = do
              set <- settings
              putStrLn $ fst set
              initLogger $   logger( snd set)
              
main = do
        getSetting        
        logI "todo.txt" "bot start"
         
       
lo= snd set
initLogger  config = putStrLn $ pathToLog config 
                      
--v= Config.logger snd(getSetting)   
{-   
fromList [("ip",String "127.0.0.1"),("social",Object
            (fromList [("GitHub",Object 
               (fromList [("blog",String "https://github.com/denisshevchenko/blog"),
                  ("ohaskell",String "https://github.com/denisshevchenko/ohaskell")]))])),
                        ("anybody",String "iouoiuio"),("port",Number 3000.0)]                           
  -}                               
