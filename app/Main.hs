module Main where
import Logger
import Config
import Exception

import Control.Exception.Base (catch)      
import Utility
             
main =  do
             logI "./out/bot.log" "bot start"
             cfg <-  readConfig
             putStrLn $ show (fst cfg)
              
getConfig $ Just (snd cfg)               
        
         
       
 
                      
--v= Config.logger snd(getSetting)   
{-   
fromList [("ip",String "127.0.0.1"),("social",Object
            (fromList [("GitHub",Object 
               (fromList [("blog",String "https://github.com/denisshevchenko/blog"),
                  ("ohaskell",String "https://github.com/denisshevchenko/ohaskell")]))])),
                        ("anybody",String "iouoiuio"),("port",Number 3000.0)]                           
  -}                               
