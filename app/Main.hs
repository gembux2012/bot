{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Main where
import Logger
import Config
            

main = do 
   logI "todo.txt" "bot start"
   readJson

  
{-   
fromList [("ip",String "127.0.0.1"),("social",Object
            (fromList [("GitHub",Object 
               (fromList [("blog",String "https://github.com/denisshevchenko/blog"),
                  ("ohaskell",String "https://github.com/denisshevchenko/ohaskell")]))])),
                        ("anybody",String "iouoiuio"),("port",Number 3000.0)]                           
  -}                               
