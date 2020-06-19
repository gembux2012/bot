module Main where
import Logger
import Config
import Exception

import Control.Exception.Base (catch)      

              
main =  do
             logI "./out/bot.log" "bot start"
             config <- readConfig
             putStrLn $ fst config
             let cfg =  snd config
             return ()
             --let l g = Logger 
             putStrLn  $  show $ logger cfg 
        
         
       
 
                      
--v= Config.logger snd(getSetting)   
{-   
fromList [("ip",String "127.0.0.1"),("social",Object
            (fromList [("GitHub",Object 
               (fromList [("blog",String "https://github.com/denisshevchenko/blog"),
                  ("ohaskell",String "https://github.com/denisshevchenko/ohaskell")]))])),
                        ("anybody",String "iouoiuio"),("port",Number 3000.0)]                           
  -}                               
